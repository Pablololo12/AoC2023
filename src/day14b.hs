module Day14b (solve) where

import qualified Data.Text as T
import Data.List
import Debug.Trace
import Data.MemoTrie as Memo

swiftColumns :: [Int] -> [Int]
swiftColumns [] = []
swiftColumns (x:xs)
  | x<2 = (take count $ repeat 1)++(take ((length list)-count) $ repeat 0)++(swiftColumns (dropWhile (<2) (x:xs)))
  | otherwise = x:(swiftColumns xs)
    where
        count = foldr (+) 0 list
        list = takeWhile (<2) (x:xs)

getMatrices :: [T.Text] -> [[Int]]
getMatrices (x:xs) = (map (\w -> if w=='.' then 0 else if w=='#' then 2 else 1) (T.unpack x)) : getMatrices xs
getMatrices [] = []

calcLoad :: [Int] -> Int
calcLoad x = foldr (\(y,z) j -> if z==2 then j else (y*z)+j) 0 $ zip [1..] $ reverse x

swiftMatrix :: [[Int]] -> [[Int]]
swiftMatrix x = map (swiftColumns) x

cyc :: [[Int]] -> [[Int]]
cyc = Memo.memo cyc'
  where
    cyc' x = transpose $ map (reverse) $ swiftMatrix $ map (reverse) $ transpose $ map (reverse) $ swiftMatrix $ map (reverse) $ transpose $ swiftMatrix $ transpose $ swiftMatrix x

loop :: Int -> [[Int]] -> [Int]
loop i x
  | i==2000 = []
  | otherwise = (foldr (+) 0 $ map (calcLoad) rotated):loop (i+1) rotated
    where
      rotated = cyc x

findPatternAux :: [Int] -> [Int] -> Bool
findPatternAux p x = (concat $ take 10 $ repeat p) == (take len x)
  where
    len = 10*(length p)

findPattern :: [Int] -> [Int] -> [Int]
findPattern x (y:ys)
  | found = x
  | otherwise = findPattern (x++[y]) ys
  where
    found = findPatternAux x (y:ys)

solve :: T.Text -> Int
solve x = head $ drop 999999999 $ ((drop 1000 listofloads)++(concat $ take 1000000000 $ repeat pat))
  where
    pat = findPattern [(listofloads !! 1000)] (drop 1001 listofloads)
    listofloads = loop 0 $ transpose matrices
    matrices = getMatrices (T.lines x)
