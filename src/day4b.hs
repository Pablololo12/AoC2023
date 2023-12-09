module Day4b (solve) where

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
calculateScore x y = L.length (reduce x y)

checkLine :: Text -> Int
checkLine x = calculateScore (parseNums (T.strip (L.head y))) (parseNums (T.strip (L.last y)))
    where y = T.split (=='|') x

removeid :: Text -> Int
removeid x = checkLine (L.last (T.split (==':') x))

accpar :: [(Int, Int)] -> Int
accpar [] = 1
accpar (x:xs) = (snd x) + (accpar xs)

calcchild :: [(Int, Int)] -> Int -> (Int, Int)
calcchild x y = (y, accpar (L.take (y) x))

tree :: [Int] -> [(Int, Int)]
tree [] = []
tree (x:xs) = (calcchild y x) : y
    where y = tree xs

doAlgo1 :: [Text] -> [Int]
doAlgo1 x = B.map (removeid) x

doAlgo :: [Text] -> [Int]
doAlgo x = snd (L.unzip(tree (doAlgo1 x)))

solve :: Text -> Int
solve x = L.foldl (+) 0 $ doAlgo $ T.lines x