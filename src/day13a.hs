module Day13a (solve) where

import qualified Data.Text as T
import Debug.Trace
import Data.List

checkMirrorAux :: [[Int]] -> [[Int]] -> Int -> Int
checkMirrorAux (a:as) (b:bs) i
  | a==b = checkMirrorAux as bs i
  | otherwise = 0
checkMirrorAux _ _ i = i

checkMirror :: [[Int]] -> [[Int]] -> Int -> Int
checkMirror (a:b:x) f i
  | a==b = let r=(checkMirrorAux (reverse f) (x) i) in if r==0 then (checkMirror (b:x) (f++[a]) (i+1)) else r
  | otherwise = checkMirror (b:x) (f++[a]) (i+1)
checkMirror _ _ _ = 0

checkEither :: [[Int]] -> Int
checkEither x = (horizontal*100)+vertical
    where
        horizontal = checkMirror x [] 1
        vertical = checkMirror (transpose x) [] 1

getMatrices :: [T.Text] -> [[[Int]]] -> [[[Int]]]
getMatrices (x:xs) (a:as)
  | x==(T.pack "") = getMatrices xs ([]:a:as)
  | otherwise = getMatrices xs ((a++[(map (\w -> if w=='.' then 0 else 1) (T.unpack x))]):as)
getMatrices [] a = a

solve :: T.Text -> Int
solve x = foldr (+) 0 $ map (checkEither) matrices
    where
        matrices = getMatrices (T.lines x) [[]]
