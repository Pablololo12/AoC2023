module Day13b (solve) where

import qualified Data.Text as T
import Data.List
import Data.Bits

hammondEqual :: [Int] -> [Int] -> Bool
hammondEqual a b = x<=1
        where
            x = foldr (+) 0 $ map (\(i,j) -> xor i j) $ zip a b

checkMirrorAux :: [[Int]] -> [[Int]] -> Int -> ([Int]->[Int]->Bool) -> Int
checkMirrorAux (a:as) (b:bs) i f
    | f a b = checkMirrorAux as bs i f
    | otherwise = 0
checkMirrorAux _ _ i _ = i

checkMirror :: [[Int]] -> [[Int]] -> Int -> Int -> ([Int]->[Int]->Bool) -> Int
checkMirror (a:b:x) f i h e
    | i==h = checkMirror (b:x) (f++[a]) (i+1) h e
    | otherwise = if (e a b) then let r=(checkMirrorAux (reverse f) (x) i e) in if r==0 then (checkMirror (b:x) (f++[a]) (i+1) h e) else r else checkMirror (b:x) (f++[a]) (i+1) h e
checkMirror _ _ _ _ _ = 0

checkEither :: [[Int]] -> Int
checkEither x = horizontal*100+vertical
        where
            horizontal = checkMirror x [] 1 horizontalOld (hammondEqual)
            vertical = checkMirror (transpose x) [] 1 verticalOld (hammondEqual)
            horizontalOld = checkMirror x [] 1 0 (==)
            verticalOld = checkMirror (transpose x) [] 1 0 (==)

getMatrices :: [T.Text] -> [[[Int]]] -> [[[Int]]]
getMatrices (x:xs) (a:as)
  | x==(T.pack "") = getMatrices xs ([]:a:as)
  | otherwise = getMatrices xs ((a++[(map (\w -> if w=='.' then 0 else 1) (T.unpack x))]):as)
getMatrices [] a = a

solve :: T.Text -> Int
solve x = foldr (+) 0 $ map (checkEither) matrices
    where
        matrices = getMatrices (T.lines x) [[]]
