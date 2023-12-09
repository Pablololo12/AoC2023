module Day2b (solve) where

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

cRed :: Int -> String -> Int
cRed x "red" = x
cRed _ _ = 0

getRed :: [Text] -> Int
getRed (x:xs) = (cRed (read (T.unpack (L.head (T.split (==' ') x)))) (T.unpack (L.last (T.split (==' ') x)))) + (getRed xs)
getRed [] = 0

cGreen :: Int -> String -> Int
cGreen x "green" = x
cGreen _ _ = 0

getGreen :: [Text] -> Int
getGreen (x:xs) = (cGreen (read (T.unpack (L.head (T.split (==' ') x)))) (T.unpack (L.last (T.split (==' ') x)))) + (getGreen xs)
getGreen [] = 0

cBlue :: Int -> String -> Int
cBlue x "blue" = x
cBlue _ _ = 0

getBlue :: [Text] -> Int
getBlue (x:xs) = (cBlue (read (T.unpack (L.head (T.split (==' ') x)))) (T.unpack (L.last (T.split (==' ') x)))) + (getBlue xs)
getBlue [] = 0

checkC :: [Text] -> Game
checkC x = Game {red=(getRed x), green=(getGreen x), blue=(getBlue x)}

checkCase :: Text -> Game
checkCase x = checkC (B.map (T.strip) (T.split (==',') x))

maxGame :: Game -> Game -> Game
maxGame a b = Game {red=if (red a)>(red b) then (red a) else (red b),
                    green=if (green a)>(green b) then (green a) else (green b),
                    blue=if (blue a)>(blue b) then (blue a) else (blue b)}

mul :: Game -> Int
mul g = (red g) * (green g) * (blue g)

checkCases :: Text -> Int
checkCases x = mul (L.foldl (maxGame) Game{red=0,green=0,blue=0} (B.map (checkCase) (T.split (==';') (L.last (T.split (==':') x)))))

doAlgo :: [Text] -> [Int]
doAlgo x = B.map (checkCases) x

solve :: Text -> Int
solve x = L.foldl (+) 0 $ doAlgo (T.lines x)