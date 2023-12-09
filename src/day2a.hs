module Day2a (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import GHC.List as L
import GHC.Base as B

data Game = Game {red :: Int, green :: Int, blue :: Int}

folder :: Int -> Int -> Int
folder 1 0 = 0
folder 1 1 = 1
folder 0 _ = 0
folder _ _ = -1

getID :: Text -> Int
getID x = read (T.unpack (L.last (T.split (==' ') (L.head (T.split (==':') x)))))

check :: Int -> String -> Int
check x "red" = if x <= 12 then 1 else 0
check x "green" = if x <= 13 then 1 else 0
check x "blue" = if x <= 14 then 1 else 0
check _ _ = -1

checkC :: Text -> Int
checkC x = check (read (T.unpack (L.head (T.split (==' ') x)))) (T.unpack (L.last (T.split (==' ') x)))

checkCase :: Text -> Int
checkCase x = L.foldl (folder) 1 (B.map (checkC) (B.map (T.strip) (T.split (==',') x)))

checkCases :: Text -> Int
checkCases x = (getID x) * (L.foldl (folder) 1 (B.map (checkCase) (T.split (==';') (L.last (T.split (==':') x)))))

doAlgo :: [Text] -> [Int]
doAlgo x = B.map (checkCases) x

solve :: Text -> Int
solve x = L.foldl (+) 0 $ doAlgo (T.lines x)