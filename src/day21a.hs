module Day21a (solve) where

import qualified Data.Text as T
import Data.List
import Data.Maybe

data Dir = N | S | E | W deriving (Eq,Ord,Show)
type Pos = (Int,Int)
type Mapa = [[Char]]

valid :: Mapa -> Pos -> Bool
valid m (x,y) = x<maxx && x>=0 && y<maxy && y>=0 && ((m!!x)!!y/='#')
  where
    maxx = length m
    maxy = length $ head m

newDir :: Mapa -> Pos -> [Pos]
newDir m (x,y) = filter (\w -> valid m w) newdirs
  where
    newdirs = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

walk :: Mapa -> [Pos] ->Int -> [Pos]
walk _ p 0 = p
walk m p i = walk m (removeDuplicates new) (i-1)
  where
    new = concat $ map (\w -> newDir m w) p

start :: [[Char]] -> Pos
start w = (x,y)
  where
    x = fromJust $ findIndex (\z-> elem 'S' z) w
    y = fromJust $ findIndex (=='S') $ w !! x

solve :: T.Text -> Int
solve x = length $ walk mapa [s] 64
  where
    mapa = map (T.unpack) $ T.lines x
    s = start mapa
