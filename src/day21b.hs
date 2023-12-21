module Day21b (solve) where

import qualified Data.Text as T
import Data.List
import Data.Maybe
import Debug.Trace

data Dir = N | S | E | W deriving (Eq,Ord,Show)
type Pos = (Int,Int)
type Mapa = [[Char]]

valid :: Mapa -> Pos -> Bool
valid m (x,y) = ((m!!xx)!!yy/='#')
  where
    xx = abs $ mod x maxx
    yy = abs $ mod y maxy
    maxx = length m
    maxy = length $ head m

newDir :: Mapa -> Pos -> [Pos]
newDir m (x,y) = filter (\w -> valid m w) newdirs
  where
    newdirs = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

walk :: Mapa -> [Pos] -> Int -> Int -> [Pos]
walk _ p 0 a = p
walk m p i a = walk m (removeDuplicates new) (i-1) (a+1)
  where
    new = concat $ map (\w -> newDir m w) p

start :: [[Char]] -> Pos
start w = (x,y)
  where
    x = fromJust $ findIndex (\z-> elem 'S' z) w
    y = fromJust $ findIndex (=='S') $ w !! x

solve :: T.Text -> Int
solve x = traceShow (x1,x2,x3) 0
  where
    a1 = walk mapa [s] 65 0
    x1 = length a1
    a2 = walk mapa a1 131 0
    x2 = length a2
    a3 = walk mapa a2 131 0
    x3 = length a3
    mapa = map (T.unpack) $ T.lines x
    s = start mapa
