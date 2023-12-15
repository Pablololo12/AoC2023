module Day15a (solve) where

import qualified Data.Text as T
import Data.Char (ord)

step :: Int -> Char -> Int
step i c = (17*(i + (ord c))) `mod` 256

hash :: T.Text -> Int
hash x = T.foldl (\a c -> step a c) 0 x

solve :: T.Text -> Int
solve x = foldr (+) 0 $ map (hash) $ T.split (==',') $ T.strip x
