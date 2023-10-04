            
{-# LANGUAGE FlexibleInstances #-}
module MP5b where

import Data.Maybe
import Prelude hiding (lines)
import Data.Ord
import Data.List hiding (lines)
import Data.List.Split (chunksOf)
import Data.Tree
import Data.Map (Map, empty, fromList, (!), findWithDefault, member, insert, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Concurrent
import Control.Monad.State
import System.IO
import System.Console.ANSI
import GHC.IO

{- I decided to implement Connect 4 game for part b. The best way to test it would be 
  to run playAI and play :) (run stack ghci to gain access to MP5b.hs-}

{- Replace with your own game data types  -}

data Piece = R | Y deriving (Eq, Show, Read)
data Board = Board [Maybe Piece]

instance Show Board where
  show (Board ps) = (intercalate "\n" $ map (concat . intersperse " " ) $ reverse $ chunksOf 7 $ map showP ps) ++ "\n0 1 2 3 4 5 6"
    where 
      showP Nothing = "-"
      showP (Just p) = show p

{- Some convenience functions and types -- feel free to replace/modify  -}

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune _ (Node x []) = Node x []
prune n (Node x ns) = Node x $ map (prune (n-1)) ns


data Scored a = Scored { score :: Int, scoredVal :: a }


instance Eq (Scored a) where
  (Scored x _) == (Scored y _) = x == y


instance Ord (Scored a) where
  compare (Scored x _) (Scored y _) = compare x y


instance Show a => Show (Scored a) where 
  show (Scored s v) = "Score: " ++ show s ++ "\n\n" ++ show v

instance Bounded (Scored a) where
  minBound = Scored minBound undefined
  maxBound = Scored maxBound undefined

minimax_ab :: (a -> Scored a) -> Tree a -> Scored a
minimax_ab scorefn (Node _ ns) = maximize minBound maxBound ns
  where maximize a b [] = minBound
        maximize a b (n:ns) | a > b = a
                            | otherwise = let sn = eval a b minimize n
                                              a' = max a sn
                                          in max sn $ maximize a' b ns
        minimize a b [] = maxBound
        minimize a b (n:ns) | a > b = b
                            | otherwise = let sn = eval a b maximize n
                                              b' = min b sn
                                          in min sn $ minimize a b' ns
        eval a b f (Node x []) = scorefn x
        eval a b f (Node x ns) = let Scored s _ = f a b ns
                                 in Scored s x

-- Minimax function from lecture notes
minimax :: (a -> Scored a) -> Tree a -> Scored a
minimax scorefn (Node _ ns) = maximize ns
  where maximize = maximumBy (comparing score) . map (eval minimize)
        minimize = minimumBy (comparing score) . map (eval maximize)
        eval _ (Node x []) = scorefn x
        eval f (Node x ns) = let Scored s _ = f ns in Scored s x


opponent :: Piece -> Piece
opponent R = Y
opponent Y = R

emptyBoard :: Board
emptyBoard = Board $ replicate 42 Nothing

nextMove :: Board -> [Int]
nextMove b@(Board ps) | won b || draw b = []
                      | otherwise = map head $ group $ sort $ map (`mod` 7) $ findIndices (== Nothing) ps

playMove :: Int -> Board -> Board
playMove n b@(Board ps) =   let idx = getIdx b n
                                (pre,_:post) = splitAt idx ps
                            in Board $ pre ++ [Just (turn b)] ++ post 

getIdx :: Board -> Int -> Int
getIdx b@(Board ps) c = idxx ps c
                        where
                          idxx ps' c' = do case ps' !! c' of
                                            (Just _ ) -> idxx ps (c'+7)
                                            _ -> c'


playMoves :: [Int] -> Board -> Board
playMoves [] b = b
playMoves (x:xs) b = playMoves xs (playMove x b)


lines :: Board -> [[Maybe Piece]]
lines b@(Board ps) =  let rows = chunksOf 7 ps
                          cols = transpose rows
                          aRows = horz rows
                          aCols = vert cols
                          aDiag = cross ps 0
                      in concat [aRows, aCols, aDiag]

          where
            horz [] = []
            horz (l@(x:xs):xss) = if length l >= 4
                                  then take 4 l : horz (xs:xss)
                                  else horz xss

            vert [] = []
            vert (l@(x:xs):xss) = if length l >= 4
                                  then take 4 l : vert (xs:xss)
                                  else vert xss

            cross _ 42 = []
            cross l idx | idx `mod` 7 == 0 && idx < 21 = 
                          [(l !! idx),(l !! (idx+8)),(l !! (idx+16)),(l !! (idx+24))] : cross l (idx+1)
                        |idx `mod` 7 == 1 && idx < 22 = 
                          [(l !! idx),(l !! (idx+8)),(l !! (idx+16)),(l !! (idx+24))] : cross l (idx+1)
                        |idx `mod` 7 == 2 && idx < 23 = 
                          [(l !! idx),(l !! (idx+8)),(l !! (idx+16)),(l !! (idx+24))] : cross l (idx+1)
                        | idx `mod` 7 == 3 && idx < 24 = 
                          [(l !! idx),(l !! (idx+8)),(l !! (idx+16)),(l !! (idx+24))] : ([(l !! idx),(l !! (idx+6)),(l !! (idx+12)),(l !! (idx+18))] : cross l (idx+1))
                        | idx `mod` 7 == 4 && idx < 25 = 
                          [(l !! idx),(l !! (idx+6)),(l !! (idx+12)),(l !! (idx+18))] : cross l (idx+1)
                        |idx `mod` 7 == 5 && idx < 26 = 
                          [(l !! idx),(l !! (idx+6)),(l !! (idx+12)),(l !! (idx+18))] : cross l (idx+1)
                        |idx `mod` 7 == 6 && idx < 27 = 
                          [(l !! idx),(l !! (idx+6)),(l !! (idx+12)),(l !! (idx+18))] : cross l (idx+1)
                        | otherwise = cross l (idx+1)
                         
              

wins :: Piece -> Board -> Bool
wins p = any (all (== Just p)) . lines

won :: Board -> Bool
won b = wins R b || wins Y b

full :: Board -> Bool
full b@(Board ps) = not (any isNothing ps)

draw :: Board -> Bool
draw b = (full b) && not (won b)

turn :: Board -> Piece
turn b@(Board ps) = let rs = length $ filter (== Just R) ps
                        ys = length $ filter (== Just Y) ps
                    in if rs == ys then R else Y

scoreBoard :: Piece -> Board -> Scored Board
scoreBoard p b  | wins p b = Scored 100 b
                | wins (opponent p) b = Scored (-100) b
                | otherwise = Scored 0 b

gameTree :: Board -> Tree Board
gameTree b@(Board ps) = Node b $ map gameTree $ map (\i -> playMove (getIdx b i) b) (nextMove b)


-- Plays a game with the AI
playAI :: IO ()
playAI = play R emptyBoard
          where 
            play _ b  | wins R b = do print b 
                                      putStrLn "Red Wins!"
                      | wins Y b = do print b
                                      putStrLn "Yellow Wins!"
                      | draw b = do print b
                                    putStrLn "Draw"
            play R b = do print b
                          putStr "Enter a move: "
                          move <- readLn
                          if move `elem` (nextMove b)
                          then play Y (playMove move b)
                          else do putStrLn "Illegal Move"
                                  play R b
            play Y b = play R $ scoredVal $ minimax_ab (scoreBoard Y) (prune 2 $ gameTree b)