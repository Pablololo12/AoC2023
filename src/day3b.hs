module Day3b (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import GHC.List as L
import GHC.Base as B

isGear :: Char -> Bool
isGear g
        | g=='*' = True
        | otherwise = False

parseNum :: Int -> Text -> Int
parseNum y x
        | y==0 = read (T.unpack (T.takeWhile (isDigit) (T.drop y x)))
        | isDigit (T.index x (y-1)) = parseNum (y-1) x
        | otherwise = read (T.unpack (T.takeWhile (isDigit) (T.drop y x)))

getNumm :: Int -> Text -> Int
getNumm y z
        | isDigit (T.index z y) = parseNum y z
        | otherwise = 0

iterr :: Int -> Int -> Int -> Int -> [Text] -> [Int]
iterr x y a b z
        | x > a = []
        | y < 0 = iterr x 0 a b z
        | y > b = iterr (x+1) (y-3) a b z
        | otherwise = [(getNumm y (z !! x))] ++ (iterr x (y+1) a b z)

searchAround :: Int -> Int -> [Text] -> [Int]
searchAround x y z
        | (x == 0) && (y == 0) = iterr x y 1 1 z
        | (x == 0) && (y+1 == T.length (L.head z)) = iterr x (y-1) 1 y z
        | (x == 0) && (y <= T.length (L.head z)) = iterr x (y-1) 1 (y+1) z
        | (x+1 == L.length z) && y == 0 = iterr (x-1) y x 1 z
        | (x+1 == L.length z) && y+1 == T.length (L.head z) = iterr (x-1) (y-1) x y z
        | (x+1 == L.length z) && y <= T.length (L.head z) = iterr (x-1) (y-1) x (y+1) z
        | (y == 0) = iterr (x-1) (y) (x+1) (y+1) z
        | (y+1 == T.length (L.head z)) = iterr (x-1) (y-1) (x+1) (y) z
        | otherwise = iterr (x-1) (y-1) (x+1) (y+1) z

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (L.filter (/= x) xs)

multi :: [Int] -> Int
multi x
        | 2 == L.length x = L.foldl (*) 1 x
        | otherwise = 0

doCase :: Int -> Int -> [Text] -> Int
doCase x y z
        | isGear (T.index (z !! x) y) = multi (L.filter (/=0) (removeDuplicates (searchAround x y z)))
        | otherwise = 0

iterate :: Int -> Int -> [Text] -> Int
iterate x y z
        | x == L.length z = 0 
        | y >= T.length (L.head z) = Day3b.iterate (x+1) 0 z
        | otherwise = (doCase x y z) + (Day3b.iterate x (y+1) z)

doAlgo :: [Text] -> Int
doAlgo x = Day3b.iterate 0 0 x

solve :: Text -> Int
solve x = doAlgo $ T.lines x