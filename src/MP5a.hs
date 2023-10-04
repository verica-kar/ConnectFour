{-# LANGUAGE FlexibleInstances #-}
module MP5a where

import Prelude hiding (lines)
import Data.Maybe
import Data.Ord
import Data.List hiding (lines)
import Data.List.Split (chunksOf)
import Data.Tree
import Data.Map (Map, empty, fromList, (!), keys, elems, assocs,
                 findWithDefault, member, insert, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Concurrent
import Control.Monad.State
import System.IO
import System.Console.ANSI
import GHC.IO
--import Debug.Trace

{- I decided to implement a mall for part a. This a variation of a maze except you don't
always start or end in the same corner. For this mall you can choose which store you want to
start at. In addition, you can chose where you would like to go, such as another store. " - " 
and "/" mark the "doors" to stores. Once the "*" has made it to the goal store a "x" will mark 
the store. The best way to test it would be to run "walk _" where _ can be one of the helper 
paths from below. -}


-- Helper Paths
path1 :: Mall  
path1 = mallMap $ Mall {
  beg = allStores !! 2,
  dest = allStores !! 5,
  walkPath = [loc $ allStores !! 2],
  adjMap = empty
}

path2 :: Mall  
path2 = mallMap $ Mall {
  beg = allStores !! 10,
  dest = allStores !! 11,
  walkPath = [loc $ allStores !! 10],
  adjMap = empty
}

path3 :: Mall  
path3 = mallMap $ Mall {
  beg = allStores !! 2,
  dest = allStores !! 13,
  walkPath = [loc $ allStores !! 2],
  adjMap = empty
}

path4 :: Mall  
path4 = mallMap $ Mall {
  beg = allStores !! 22,
  dest = allStores !! 8,
  walkPath = [loc $ allStores !! 22],
  adjMap = empty
}

path5 :: Mall  
path5 = mallMap $ Mall {
  beg = allStores !! 36,
  dest = allStores !! 11,
  walkPath = [loc $ allStores !! 36],
  adjMap = empty
}

path6 :: Mall  
path6 = mallMap $ Mall {
  beg = allStores !! 1,
  dest = allStores !! 36,
  walkPath = [loc $ allStores !! 1],
  adjMap = empty
}

-- Data and Types
data Mall = Mall {
  beg :: Store,
  dest :: Store,
  walkPath :: WalkPath,
  adjMap :: Map Location [Location]
} deriving (Eq)

data Store = Store {
  name :: String,
  loc :: Location
} deriving (Eq)

data EntranceExit = EE {
  eName :: String,
  eLoc :: Location
} deriving (Eq)


type Location = (Int,Int)
type WalkPath = [Location]


allStores :: [Store]
allStores = [
  Store {name = "LULULEMON", loc = (2,4)},
  Store {name = "FOOT LOCKER", loc = (2,7)},
  Store {name = "APPLE", loc = (2,10)},
  Store {name = "NORDSTROM", loc = (2,13)},
  Store {name = "ALTER'D STATE", loc = (3,15)},
  Store {name = "NIKE", loc = (5,15)},
  Store {name = "ADIDAS", loc = (8,15)},
  Store {name = "PANDORA", loc = (12,15)},
  Store {name = "PACSUN", loc = (14,13)},
  Store {name = "H&M", loc = (14,10)},
  Store {name = "CROCS", loc = (14,5)},
  Store {name = "KOHL'S", loc = (7,2)},
  Store {name = "MACY", loc = (10,2)},
  Store {name = "J.C. PENNEY", loc = (13,2)},
  Store {name = "MAC", loc = (4,2)},
  Store {name = "BUILD-A-BEAR", loc = (4,5)},
  Store {name = "TOMMY BAHAMA", loc = (4,8)},
  Store {name = "BATH & BODY WORKS", loc = (4,11)},
  Store {name = "TARGET", loc = (5,4)},
  Store {name = "BANANA REPUBLIC", loc = (5,7)},
  Store {name = "FOREVER 21", loc = (5,13)},
  Store {name = "WINDSOR", loc = (6,9)},
  Store {name = "COACH", loc = (6,11)},
  Store {name = "PUMA", loc = (7,5)},
  Store {name = "CLAIRE'S", loc = (7,13)},
  Store {name = "CHAMPS", loc = (8,4)},
  Store {name = "URBAN OUTFITERS", loc = (8,10)},
  Store {name = "GARAGE", loc = (9,8)},
  Store {name = "LUSH", loc = (9,12)},
  Store {name = "AMERICAN EAGLE", loc = (10,5)},
  Store {name = "ARITZIA", loc = (10,13)},
  Store {name = "FOOT LOCKER KIDS", loc = (11,4)},
  Store {name = "THINGS REMEMBERED", loc = (11,7)},
  Store {name = "TORY BURCH", loc = (11,9)},
  Store {name = "KATE SPADE", loc = (12,8)},
  Store {name = "ALEX AND ANI", loc = (12,10)},
  Store {name = "SUNGLASS HUT", loc = (12,12)}]

allEntrExit :: [EntranceExit]
allEntrExit = [
  EE {eName = "NORTH", eLoc = (3,1)},
  EE {eName = "EAST", eLoc = (15,7)},
  EE {eName = "SOUTH", eLoc = (9,15)},
  EE {eName = "WEST", eLoc = (1,11)}]


-- Show Instances
instance Show Mall where
  show = drawMall

instance Show Store where
  show (Store n (x,y)) = n ++ " @ " ++ "(" ++ show x ++ "," ++ show y ++")"

instance Show EntranceExit where
  show (EE n (x,y)) = n ++ " Entrance/Exit @ " ++ "(" ++ show x ++ "," ++ show y ++")"


-- Build Mall
drawMall :: Mall -> String
drawMall m = (concat $ map drawRow $ chunksOf 15 drawnCells) ++ bot
              where
                drawRow cs =  let (l1,l2) = unzip cs
                              in concat l1 ++ "+\n" ++ concat l2 ++ "|\n"
                drawnCells = [drawWallNW (x,y) m | y <- [1..15], x <- [1..15]]
                bot = (concat $ replicate 15 "+---") ++ "+"

drawWallNW :: Location -> Mall -> (String,String)
drawWallNW c@(x,y) (Mall _ _ p cmap) =  let nc = (x,y-1)
                                            wc = (x-1,y)
                                            adj = findWithDefault [] c cmap
                                        in ("+" ++  if (nc `elem` locs && nc `elem` adj) || (c `elem` locs && nc `elem` adj)
                                                    then " - " 
                                                    else (if nc `elem` adj 
                                                          then "   " 
                                                          else "---"),
                                            ( if (wc `elem` locs && wc `elem` adj) || (c `elem` locs && wc `elem` adj)
                                              then "/" 
                                              else (if wc `elem` adj 
                                                    then " " 
                                                    else "|"))
                                              ++ (if head p == c 
                                                  then " x " 
                                                  else (if c `elem` p 
                                                        then " * " 
                                                        else "   ")))
                                        where
                                          locs = [ loc s | s <- allStores ]



openWall :: Location -> Location -> Mall -> Mall
openWall l1 l2 m@(Mall _ _ _ cmap) = m { adjMap = insertWith (++) l1 [l2] $ insertWith (++) l2 [l1] cmap}

mallMap :: Mall -> Mall
mallMap m = build locs m
            where
              build [l1] ma = ma
              build (l1:l2:ls) ma = build (l2:ls) $ openWall l1 l2 ma
              locs = [(6,3),(5,3),(4,3),(4,2),(4,3),(3,3),(3,2),(3,1),(3,2),(3,3),(3,4),
                      (2,4),(3,4),(3,5),(4,5),(3,5),(3,6),(3,7),(2,7),(3,7),(3,8),(4,8),
                      (3,8),(3,9),(3,10),(3,11),(2,11),(1,11),(2,11),(2,10),(2,11),(3,11),
                      (3,12),(3,13),(2,13),(3,13),(3,14),(3,15),(3,14),(4,14),(5,14),
                      (5,13),(5,14),(5,15),(5,14),(6,14),(7,14),(7,13),(7,14),(8,14),(8,15),
                      (8,14),(9,14),(9,15),(9,14),(10,14),(10,13),(10,14),(11,14),(12,14),
                      (12,15),(12,14),(13,14),(13,13),(14,13),(13,13),(13,12),(12,12),
                      (13,12),(13,11),(13,10),(12,10),(13,10),(14,10),(13,10),(13,9),
                      (13,8),(12,8),(13,8),(13,7),(14,7),(15,7),(14,7),(13,7),(13,6),
                      (13,5),(14,5),(13,5),(13,4),(13,3),(13,2),(13,3),(12,3),(11,3),
                      (11,4),(11,3),(10,3),(9,3),(8,3),(8,4),(8,3),(7,3),(7,2),(7,3),
                      (6,3),(6,4),(5,4),(6,4),(6,5),(7,5),(6,5),(6,6),(6,7),(5,7),(6,7),
                      (6,8),(7,8),(7,9),(6,9),(7,9),(7,10),(7,11),(6,11),(7,11),(8,11),
                      (8,10),(8,11),(9,11),(9,12),(9,11),(10,11),(11,11),(12,11),(13,11),
                      (12,11),(11,11),(10,11),(10,10),(10,9),(11,9),(10,9),(10,8),(9,8),
                      (10,8),(10,7),(11,7),(10,7),(10,6),(11,6),(12,6),(13,6)]


-- Search function from lecture notes
search :: (Eq a, Show a) =>
          (a -> Bool)
          -> (a -> [a])
          -> ([a] -> [a] -> [a])
          -> [a] -> [a]
          -> Maybe a
search goal adj comb unvisited visited
  | null unvisited = Nothing
  | goal (head unvisited) = Just (head unvisited)
  | otherwise = let (n:ns) = unvisited
                in debug n $ -- uncomment to "debug"
                   search goal adj comb
                          (comb (removeDups (adj n)) ns)
                          (n:visited)
  where removeDups = filter (not . (`elem` (unvisited ++ visited)))


debug :: Show a => a -> b -> b
debug x y = unsafePerformIO clearScreen `seq`
            unsafePerformIO (setCursorPosition 0 0) `seq`
            unsafePerformIO (putStrLn $ show x) `seq`
            unsafePerformIO (threadDelay $ 3*10^5) `seq`
            y


-- Call with an admissible heuristic as the cost function to carry out A* search
bestFirstSearch :: (Eq a, Show a, Ord b) =>
                   (a -> Bool)
                   -> (a -> [a])
                   -> (a -> b)
                   -> a -> Maybe a
bestFirstSearch goal succ cost start = search goal succ comb [start] []
  where comb new old = sortOn cost (new ++ old)




-- "Solve"
nextPaths :: Mall -> [Mall]
nextPaths m@(Mall _ (Store _ (w,z)) p@(loc:_) cmap) = do  adj <- filter (not . flip elem p) $ findWithDefault [] loc cmap
                                                          return $ m { walkPath = adj:p }
                                            
                                            
walk :: Mall -> Maybe Mall
walk ma =  walkMall cost ma
            where 
              cost m@(Mall _ (Store _ (w,z)) (p@(x,y):_) _) = abs (w-x) + abs (z-y) + length p


walkMall :: Ord a => (Mall -> a) -> Mall -> Maybe Mall
walkMall cost m@(Mall (Store _ (x,y)) (Store _ (w,z)) _ _) =  let start = (x,y)
                                                                  end = (w,z)
                                                              in bestFirstSearch ((== end) . head . walkPath)
                                                                                  nextPaths
                                                                                  cost
                                                                                  (m { walkPath = [start]})

