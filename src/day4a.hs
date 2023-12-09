module Day4a (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import GHC.List as L
import GHC.Base as B

parsn :: Text -> Int
parsn x = read (T.unpack x)

parseNums :: Text -> [Int]
parseNums x = L.map (parsn) (L.filter (not . T.null) (T.split (==' ') x))

reduce :: [Int] -> [Int] -> [Int]
reduce [] _ = []
reduce (x:xs) y
      | x `L.elem` y = x : reduce xs y
      | otherwise = reduce xs y

calculateScore :: [Int] -> [Int] -> Int
calculateScore x y = if z==0 then 0 else 2^(z-1)
    where z = L.length (reduce x y)

checkLine :: Text -> Int
checkLine x = calculateScore (parseNums (T.strip (L.head y))) (parseNums (T.strip (L.last y)))
    where y = T.split (=='|') x

removeid :: Text -> Int
removeid x = checkLine (L.last (T.split (==':') x))

doAlgo :: [Text] -> [Int]
doAlgo x = B.map (removeid) x

solve :: Text -> Int
solve x = L.foldl (+) 0 $ doAlgo $ T.lines x