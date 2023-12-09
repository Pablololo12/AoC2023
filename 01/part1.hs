module Day1a (solve) where

import Data.Char
import Data.Text.IO
import GHC.Base as B
import GHC.List as L
import Data.Text as T
import GHC.List
import Debug.Trace

getNum :: String -> Int
getNum [] = 0
getNum (x:xs)
       | isNumber x = digitToInt x
       | otherwise = getNum xs

doAlgo :: String -> Int
doAlgo x = j*10 + k
    where j = getNum x
          k = getNum $ L.reverse x

solve :: Text -> Int
solve x = GHC.List.foldl (+) 0 (B.map (doAlgo . unpack) (T.lines x))

main :: IO()
main = do
    content <- Data.Text.IO.readFile "input.txt"
    print $ solve content
