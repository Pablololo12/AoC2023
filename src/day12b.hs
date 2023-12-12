module Day12b (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import Data.List as D
import GHC.List as L
import GHC.Base as B
import qualified Data.Map as M
import Debug.Trace
import Data.MemoTrie as Memo

increase :: ([Char],[Int]) -> ([Char],[Int])
increase (x,y) = (x++['?']++x++['?']++x++['?']++x++['?']++x,y++y++y++y++y)

splitnparse :: Text -> ([Char],[Int])
splitnparse x = (unpack (L.head p), y)
    where
      y = L.map (read . unpack) $ T.split (==',') (L.last p)
      p = T.split (==' ') x

step :: [Char] -> [Int] -> Int
step = Memo.memo2 step'
    where
      step' xs []
        | L.all (`L.elem` ".?") xs = 1
        | otherwise = 0
      step' [] _ = 0
      step' ('.':xs) i = step xs i
      step' ('#':xs) (y:ys)
        | ((L.length ws)==0) && ((L.length w)==(y-1)) && (L.all (`L.elem` "#?") w) = step [] ys
        | (L.length w)==(y-1) && L.all (`L.elem` "#?") w && ((L.head ws) `L.elem` "?.") = step (L.tail ws) ys
        | otherwise = 0
        where
          (w,ws)=L.splitAt (y-1) xs
      step' ('?':xs) i = (step ('#':xs) i)+(step ('.':xs) i)
      
solve :: Text -> Int
solve x = L.foldl (+) 0 $ L.map (\w -> step (fst w) (snd w)) increased
    where
      increased = L.map (increase) divided
      divided = L.map (splitnparse) lines
      lines = T.lines x
