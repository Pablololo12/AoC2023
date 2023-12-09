module Day8b (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import Data.List as D
import GHC.List as L
import GHC.Base as B
import Data.Map as M

iter :: [Int] -> [Int] -> Int -> Text -> Map Text (Text,Text) -> [Int]
iter f [] c w m = iter f f c w m
iter f (x:xs) c w m
    | (T.last w)=='Z' && x==0 = c : (iter f xs (c+1) (fst (m ! w)) m)
    | (T.last w)=='Z' && x==1 = c : (iter f xs (c+1) (snd (m ! w)) m)
    | x == 0 = (iter f xs (c+1) (fst (m ! w)) m)
    | x == 1 = (iter f xs (c+1) (snd (m ! w)) m)

maptoint :: String -> [Int]
maptoint [] = []
maptoint (x:xs) = j : maptoint xs
    where j = if x=='R' then 1 else 0

prepLines :: [Text] -> [(Text, (Text, Text))]
prepLines [] = []
prepLines (x:xs) = (a,(L.head b, L.last b)) : prepLines xs
    where a = L.head c
          b = T.split (==',') (T.filter (\w -> ((w/='(') && (w/=')'))) (L.last c))
          c = T.split (=='=') (T.filter (/=' ') x)

lcmm :: [Int] -> Int
lcmm [] = 1
lcmm (x:xs) = lcm x (lcmm xs)

doAlgo :: [Text] -> Int
doAlgo (x:xs) = lcmm (L.map (L.head) a)
    where a = B.map (\y -> iter j j 0 y w) m
          j = maptoint (unpack x)
          m = L.filter (\y -> (T.last y)=='A') (B.map (T.filter (/=' ')) (B.map (L.head . (T.split (=='='))) xs))
          w = fromList (prepLines xs)

solve :: Text -> Int
solve x = doAlgo $ L.filter (not . T.null) $ T.lines x