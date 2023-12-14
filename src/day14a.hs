module Day14a (solve) where

import qualified Data.Text as T
import Data.List

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

solve :: T.Text -> Int
solve x = foldr (+) 0 $ map (calcLoad) $ map (swiftColumns) $ transpose matrices
  where
    matrices = getMatrices (T.lines x)
    temp = map (swiftColumns) $ transpose matrices
