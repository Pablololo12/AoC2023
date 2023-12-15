module Day15b (solve) where

import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (ord)
import Data.Maybe

step :: Int -> Char -> Int
step i c = (17*(i + (ord c))) `mod` 256

hash :: T.Text -> Int
hash x = T.foldl (\a c -> step a c) 0 x

addorupdate :: [(T.Text,Int)] -> (T.Text,Int) -> [(T.Text,Int)]
addorupdate x (a,b)
  | index>=0 =  (take index x) ++ [(a,b)] ++ (drop (index+1) x)
  | otherwise =  x ++ [(a,b)]
  where
    index = (fromMaybe (-1) (L.findIndex (\(c,d) -> c==a) x))

handleCase :: [[(T.Text,Int)]] -> T.Text -> [[(T.Text,Int)]]
handleCase x c
  | remove =  (take index x) ++ [(L.deleteBy (\(k,l) (d,f) -> k==d) (indexC,0) $ head $ drop index x)] ++ (drop (index+1) x)
  | otherwise =  (take index x) ++ [(addorupdate (head $ drop index x) (indexC,value))] ++ (drop (index+1) x)
  where
    value = if (not remove) then read (T.unpack $ last $ T.split (=='=') $ c) :: Int else 0
    remove = T.any (=='-') c
    index = hash $ head $ T.split (=='-') $ head $ T.split(=='=') c
    indexC = head $ T.split (=='-') $ head $ T.split(=='=') c
    temp = head $ T.split (=='-') $ head $ T.split(=='=') c

computeFocusPower :: [[(T.Text,Int)]] -> Int -> Int -> Int
computeFocusPower [] _ acum = acum
computeFocusPower (x:xs) i acum = computeFocusPower xs (i+1) (acum+(i*(foldl (+) 0 $ map (\(a,b) -> a*(snd b)) $ zip [1..] x)))

solve :: T.Text -> Int
solve x = computeFocusPower (foldl (handleCase) (replicate 256 []) (T.split (==',') $ T.strip x)) 1 0
