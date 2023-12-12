module Day12a (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import Data.List as D
import GHC.List as L
import GHC.Base as B
import qualified Data.Map as M
import Debug.Trace

splitnparse :: Text -> ([Char],[Int])
splitnparse x = (unpack (L.head p), y)
    where
      y = L.map (read . unpack) $ T.split (==',') (L.last p)
      p = T.split (==' ') x

reduce :: [Char] -> [Int] -> Int -> [Int]
reduce [] l i = if i>0 then l++[i] else l
reduce ('#':xs) l i = reduce xs l (i+1)
reduce ('.':xs) l i = reduce xs (l++[i]) 0

valid :: [Char] -> [Int] -> Bool
valid x y = y==z
    where
      z = L.filter (/=0) $ reduce x [] 0

step :: [Char] -> [Char] -> [Int] -> Int
step b [] n = if (valid b n) then 1 else 0
step b ('?':xs) n = (step (b++['#']) xs n) + (step (b++['.']) xs n)
step b (x:xs) n = step (b++[x]) xs n

solve :: Text -> Int
solve x = L.foldl (+) 0 $ L.map (\w -> step [] (fst w) (snd w)) $ L.map (splitnparse) lines
    where
      lines = T.lines x
