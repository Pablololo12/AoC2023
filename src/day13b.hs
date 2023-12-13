module Day13b (solve) where

import qualified Data.Text as T
import Debug.Trace
import Data.List

checkMirrorAux :: [[Char]] -> [[Char]] -> Int -> Int
checkMirrorAux (a:as) (b:bs) i
    | a==b = checkMirrorAux as bs i
    | otherwise = 0
checkMirrorAux _ _ i = i

checkMirror :: [[Char]] -> [[Char]] -> Int -> Int
checkMirror (a:b:x) f i = if a==b then let r=(checkMirrorAux (reverse f) (x) i) in if r==0 then (checkMirror (b:x) (f++[a]) (i+1)) else r else checkMirror (b:x) (f++[a]) (i+1)
checkMirror x y z = 0
checkMirror [] _ _ = 0

checkEither :: [[Char]] -> Int
checkEither x = (horizontal*100)+vertical
    where
        horizontal = checkMirror x [] 1
        vertical = checkMirror (transpose x) [] 1

getMatrices :: [T.Text] -> [[[Char]]] -> [[[Char]]]
getMatrices (x:xs) (a:as)
    | x==(T.pack "") = getMatrices xs ([]:a:as)
    | otherwise = getMatrices xs ((a++[(T.unpack x)]):as)
getMatrices [] a = a
getMatrices (x:xs) [] = getMatrices xs [[(T.unpack x)]]

adder :: Int -> Int -> Int
adder b a = (a+b)

solve :: T.Text -> Int
solve x = foldr (adder) 0 $ map (checkEither) matrices
    where
        matrices = getMatrices (T.lines x) []
