module Day12b (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import Data.List as D
import GHC.List as L
import GHC.Base as B
import qualified Data.Map as M
import Debug.Trace

increase :: ([Char],[Int]) -> ([Char],[Int])
increase (x,y) = (x++['?']++x++['?']++x++['?']++x++['?']++x,y++y++y++y++y)

splitnparse :: Text -> ([Char],[Int])
splitnparse x = (unpack (L.head p), y)
    where
      y = L.map (read . unpack) $ T.split (==',') (L.last p)
      p = T.split (==' ') x

reduce :: [Char] -> [Int] -> Int -> [Int]
reduce [] l i = if i>0 then l++[i] else l
reduce ('#':xs) l i = reduce xs l (i+1)
reduce ('.':xs) l i = reduce xs (l++[i]) 0
reduce ('?':xs) l i = if i>0 then l++[i] else l

valid :: [Char] -> [Int] -> Bool
valid x y = y==z
    where
      z = L.filter (/=0) $ reduce x [] 0

validSoFar :: [Char] -> [Int] -> Bool
validSoFar x y = if (L.length z)==0 then True else (L.init w)==(L.init z)
    where
      w = L.take (L.length z) y
      z = L.filter (/=0) $ reduce x [] 0

step :: [Char] -> [Char] -> [Int] -> Int
step b [] n = if (valid b n) then 1 else 0
step b (x:xs) n
    | not $ validSoFar b n = 0
    | x=='?' = (step (b++['#']) xs n) + (step (b++['.']) xs n)
    | otherwise = step (b++[x]) xs n

adder :: Int -> Int -> Int
adder b a = traceShow (b) b+a

solve :: Text -> Int
solve x = L.foldl (adder) 0 $ L.map (\w -> step [] (fst w) (snd w)) increased
    where
      increased = L.map (increase) divided
      divided = L.map (splitnparse) lines
      lines = T.lines x
