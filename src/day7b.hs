module Day7b (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import Data.List as D
import GHC.List as L
import GHC.Base as B
import Data.Maybe
import Data.Map

getNum :: Char -> Int
getNum x = fromList [('A',14),('K',13),('Q',12),('T',10),('9',9),('8',8),('7',7),('6',6),('5',5),('4',4),('3',3),('2',2),('J',1)] ! x

specAccum :: Int -> (Text,Int,Int) -> Int
specAccum x (a,b,c) = x + (c*b)

removeDuplicates :: Eq a => [a] -> [(a,Int)]
removeDuplicates [] = []
removeDuplicates (x:xs) = (x,((L.length xs)-(L.length j))+1) : removeDuplicates j
    where j = L.filter (/=x) xs

parse :: Text -> (Text,Int)
parse x = (j!!0,read(unpack(j!!1)))
    where j = T.split (==' ') x

countJ :: [(Char, Int)] -> Int
countJ x = snd (fromMaybe ('m',0) j)
    where j = D.find (\w -> (fst w)=='J') x

isFive :: [(Char, Int)] -> Bool
isFive x = (L.length x) == 1

isFour :: [(Char, Int)] -> Bool
isFour (x:y:_) = ((snd x) == 4) || ((snd y) == 4)
isFour _ = False

isFullH :: [(Char, Int)] -> Bool
isFullH (x:y:_) = (((snd x) == 3) && ((snd y) == 2)) || (((snd y) == 3) && ((snd x) == 2))
isFullH _ = False

isTrio :: [(Char, Int)] -> Bool
isTrio [] = False
isTrio (x:xs) = ((snd x) == 3) || (isTrio xs)

countpair :: [(Char, Int)] -> Int
countpair [] = 0
countpair (x:xs) = j + (countpair xs)
    where j = if (snd x) == 2 then 1 else 0

isTwoPair :: [(Char, Int)] -> Bool
isTwoPair x = (countpair x) > 1

isPair :: [(Char, Int)] -> Bool
isPair x = (countpair x) > 0

compareHigh :: [Char] -> [Char] -> Bool
compareHigh (x:xs) (y:ys) = if a==b then (compareHigh xs ys) else a>b
    where a = getNum x
          b = getNum y

getArr :: [(Char, Int)] -> [Bool]
getArr x = [(isFive x),(isFour x),(isFullH x),(isTrio x),(isTwoPair x),(isPair x)]

compareArr :: [Bool] -> [Bool] -> Text -> Text -> Bool
compareArr [] [] a b = compareHigh (unpack a) (unpack b)
compareArr (x:xs) (y:ys) a b
    | x && y = compareHigh (unpack a) (unpack b)
    | x && (not y) = True
    | y && (not x) = False
    | otherwise = compareArr xs ys a b

sumJ :: [(Char, Int)] -> [(Char, Int)]
sumJ x = [((fst a), (snd a)+c)] ++ b
    where c = countJ x
          a = L.head f
          b = L.drop 1 f
          f = L.reverse (sortOn snd (L.filter (\w -> (fst w)/='J') x))

compare :: Text -> Text -> Bool
compare a b = compareArr as bs a b
    where as = L.zipWith (||) (getArr ars) (getArr (sumJ ars))
          bs = L.zipWith (||) (getArr brs) (getArr (sumJ brs))
          ars = removeDuplicates (unpack a)
          brs = removeDuplicates (unpack b)

order :: [(Text,Int)] -> [(Text,Int)]
order [] = []
order (x:xs) = a ++ [x] ++ b
    where a = order [w | w<-xs, Day7b.compare (fst x) (fst w)]
          b = order [w | w<-xs, Day7b.compare (fst w) (fst x)]

doAlgo :: [Text] -> Int
doAlgo x = L.foldl (specAccum) 0 (L.zip3 w z [1..])
    where w = fst j 
          z = snd j
          j = L.unzip (order (B.map (parse) x))

solve :: Text -> Int
solve x = doAlgo $ T.lines x