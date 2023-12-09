module Day6 (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import GHC.List as L
import GHC.Base as B

parsn :: Text -> Int
parsn x = read (T.unpack x)

parseNums :: Text -> [Int]
parseNums x = B.map (parsn) (L.drop 1 (L.filter (not . T.null) (T.split (==' ') x)))

canBreak :: (Int,Int,Int) -> Int
canBreak (a,b,c) = if d > c then 1 else 0
    where d = a*(b-a)

algo :: (Int,Int) -> Int
algo (a,b) = L.foldl (+) 0 (B.map (canBreak) (L.zip3 [1..a] (L.take a (repeat a)) (L.take a (repeat b))))

doAlgo :: [Text] -> [Int]
doAlgo (x:y:_) = B.map (algo) (L.zip (parseNums x) (parseNums y))

solve :: Text -> Int
solve x = L.foldl (*) 1 $ doAlgo $ T.lines x