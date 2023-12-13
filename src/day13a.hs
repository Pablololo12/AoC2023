module Day13a (solve) where

import qualified Data.Text as T
import Debug.Trace

checkMirrorAux :: [[Char]] -> [[Char]] -> Int
checkMirrorAux (a:as) (b:bs)
    | a==b = checkMirrorAux as bs
    | otherwise = -1

checkMirror :: [[Char]] -> Int -> Int
checkMirror (a:b:x) i = if a==b then i else checkMirror (b:x) (i+1)
checkMirror [] _ = -1

checkEither :: [[Char]] -> Int
checkEither x
    | horizontal /= -1 = horizontal*100
    | otherwise = vertical
    where
        horizontal = checkMirror x 1
        vertical = checkMirror (traspose x) 1

getMatrices :: [T.Text] -> [[[Char]]] -> [[[Char]]]
getMatrices (x:xs) (a:as)
    | x==(T.pack "") = getMatrices xs ([]:a:as)
    | otherwise = getMatrices xs ((a++[(T.unpack x)]):as)

solve :: T.Text -> Int
solve x = foldr (+) 0 $ map (checkEither) matrices
    where
        matrices = getMatrices $ T.lines x