module Day21b (solve) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe
import Debug.Trace

type Pos = (Int,Int)
type Mapa = [[Char]]

newDir :: Pos -> [Pos]
newDir (x,y) = newdirs
  where
    newdirs = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

walk :: Mapa -> M.Map Pos Int -> [(Pos,Int)] -> [Int]
walk _ _ [] = []
walk m d ((pos@(x,y),w):xs)
  | M.member pos d || ((m!!xx)!!yy=='#') = walk m d xs
  | otherwise = w : (walk m (M.insert pos w d) (xs++(zip (newDir pos) $ repeat (w+1))))
  where
    xx = x `mod` maxx
    yy = y `mod` maxy
    maxx = length m
    maxy = length $ head m

start :: [[Char]] -> Pos
start w = (x,y)
  where
    x = fromJust $ findIndex (\z-> elem 'S' z) w
    y = fromJust $ findIndex (=='S') $ w !! x

solve :: T.Text -> Int
solve x = totals !! 26501365
  where
    mapa = map (T.unpack) $ T.lines x
    s = start mapa
    value = map length $ group $ walk mapa M.empty [(s,0)]
    size = length mapa
    diffs = zipWith (-) (drop size value) value
    diffs' = findCycle diffs
    findCycle as = if take size as == take size (drop size as) then cycle (take size as) else head as : findCycle (tail as)
    res = take size value ++ zipWith (+) res diffs'
    totals = zipWith (+) res (0:0:totals)
