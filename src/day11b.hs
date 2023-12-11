module Day11b (solve) where

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

distance :: (Int,Int) -> (Int,Int) -> Int
distance (x,y) (a,b) = (abs (a-x))+(abs (b-y))

loop :: [(Int,Int)] -> Int
loop [] = 0
loop (x:xs) = (L.foldl (+) 0 $ L.map (\w -> distance x w) xs) + (loop xs)

expandCoordinates :: [Text] -> (Int,Int) -> (Int,Int)
expandCoordinates t (x,y) = ((x+(countx*999999)),(y+(county*999999)))
    where
      countx = L.foldl (\c w -> if w then c+1 else c) 0 (L.map (emptyRow) $ L.take x t)
      county = L.foldl (\c w -> if w then c+1 else c) 0 (L.map (\w -> emptyColumn t w) $ L.take y [0..])

solve :: Text -> Int
solve x = loop $ L.map (\w -> expandCoordinates mapa w) $ findGalaxy 0 mapa
    where
      mapa = T.lines x
