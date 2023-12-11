module Day11a (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import Data.List as D
import GHC.List as L
import GHC.Base as B
import Data.Map as M
import Data.Maybe

findInRow :: Int -> Text -> [Int]
findInRow i x
    | isNothing y = []
    | isJust y = ((fromJust y)+i) : (findInRow ((fromJust y)+i+1) $ T.drop ((fromJust y)+1) x)
    where
      y = T.findIndex (=='#') x


findGalaxy :: Int -> [Text] -> [(Int,Int)]
findGalaxy _ [] = []
findGalaxy i (x:xs)
    | 0==(L.length y) = findGalaxy (i+1) xs
    | otherwise = (L.map (\w -> (i,w)) y) ++ findGalaxy (i+1) xs
    where
      y = findInRow 0 x

emptyColumn :: [Text] -> Int -> Bool
emptyColumn x i = 0 == L.length vals
    where
      vals = L.filter (=='#') column
      column = [index w i | w <- x]

emptyRow :: Text -> Bool
emptyRow x = (0 == T.length vals)
    where
      vals = T.filter (=='#') $ x

inflateRow :: [Text] -> [Text]
inflateRow [] = []
inflateRow (x:xs)
    | emptyRow x = [x,x] ++ inflateRow xs
    | otherwise = x : inflateRow xs

insertEmpty :: Text -> Int -> Text
insertEmpty x i = T.concat [(T.take i x),(pack "."),(T.drop i x)]

inflateColumn :: [Text] -> Int -> [Text]
inflateColumn x i
    | i==(T.length (L.head x)) = x
    | emptyColumn x i = inflateColumn (L.map (\w -> insertEmpty w i) x) (i+2)
    | otherwise = inflateColumn x (i+1)

increaseSize :: [Text] -> [Text]
increaseSize x = inflateRow $ inflateColumn x 0

distance :: (Int,Int) -> (Int,Int) -> Int
distance (x,y) (a,b) = (abs (a-x))+(abs (b-y))

loop :: [(Int,Int)] -> Int
loop [] = 0
loop (x:xs) = (L.foldl (+) 0 $ L.map (\w -> distance x w) xs) + (loop xs)

solve :: Text -> Int
solve x = loop $ findGalaxy 0 $ increaseSize $ T.lines x
