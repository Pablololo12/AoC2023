module Day5b (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import GHC.List as L
import GHC.Base as B

parsn :: Text -> Int
parsn x = read (T.unpack x)

headisdigit :: Text -> Bool
headisdigit x = isDigit (T.head x)

filt :: [(Int,Int)] -> [Int] -> [(Int,Int)]
filt [] x = []
filt ((f,s):xs) (a:b:c:_)
    | f<0 = (f,s) : filt xs [a,b,c]
    | (f>=b) && ((b+c)>=(f+s)) = (-((f-b)+a),s) : filt xs [a,b,c]
    | (f>=b) && (f<(b+c)) = [(-((f-b)+a),((b+c)-f)),((b+c),((f+s)-(b+c)))] ++ filt xs [a,b,c]
    | ((f+s)>=b) && ((f+s)<(b+c)) = [(f,(b-f)),(-a,((f+s)-b))] ++ filt xs [a,b,c]
    | (f<b) && ((f+s)>(b+c)) = [(f,(b-f)),((b+c),((f+s)-(b+c))),(-a,c)] ++ filt xs [a,b,c]
    | otherwise = (f,s) : filt xs [a,b,c]

filterall :: [(Int,Int)] -> [Text] -> [(Int,Int)]
filterall x [] = x
filterall x (t:ts) = filterall (filt x n) ts
    where n = B.map (parsn) (T.split (==' ') t)

abspar :: (Int,Int) -> (Int,Int)
abspar (a,b) = (abs a, b)

cases :: [(Int,Int)] -> [Text] -> [(Int,Int)]
cases x [] = x
cases x (t:ts)
    | isDigit (T.head t) = cases x ts
    | otherwise = cases (L.map (abspar) (filterall x (L.takeWhile (headisdigit) ts))) ts

generate :: [Int] -> [(Int,Int)]
generate [] = []
generate (a:b:xs) = (a,b) : (generate xs)

doAlgo :: [Text] -> [Int]
doAlgo (x:xs) = B.map (fst) (cases j xs)
  where j = generate (B.map (parsn) (L.drop 1 (T.split (==' ') x)))

solve :: Text -> Int
solve x = L.minimum $ doAlgo $ L.filter (not . T.null) $ T.lines x