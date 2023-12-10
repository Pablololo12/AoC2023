module Day10b (solve) where

import Data.Char
import Data.Text.IO
import Data.Text as T
import Data.List as D
import GHC.List as L
import GHC.Base as B
import Data.Map as M
import Data.Maybe

reduce :: [Int] -> [Int]
reduce (x:y:xs) = (y-x) : reduce (y:xs) 

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (u, v) = (x+u, y+v)

diff :: (Int, Int) -> (Int, Int) -> (Int, Int)
diff (x, y) (u, v) = (x-u, y-v)

gchar :: [Text] -> (Int,Int) -> Char
gchar t (x,y) = index ts y
    where ts = t !! x

lookaround :: [Text] -> (Int,Int) -> (Int, Int)
lookaround p (x,y)
    | up=='|' || up=='7' || up=='F' = (x-1,y)
    | down=='|' || down=='L' || down=='J' = (x+1,y)
    | left=='-' || left=='L' || left=='F' = (x,y-1)
    | right=='-' || right=='J' || right=='7' = (x,y+1)
    | otherwise = (0,0)
    where
        up = gchar p (x-1,y)
        down = gchar p (x+1,y)
        left = gchar p (x,x-1)
        right = gchar p (x,x+1)

-- Character of where we are, the pairs in this case are deltas
calcNext :: Char -> (Int,Int) -> (Int, Int)
calcNext '|' (x,y) = (x,y)
calcNext '-' (x,y) = (x,y)
calcNext 'L' (x,y) = if x==1 then (0,1) else (-1,0)
calcNext 'J' (x,y) = if x==1 then (0,-1) else (-1,0)
calcNext '7' (x,y) = if x==(-1) then (0,-1) else (1,0)
calcNext 'F' (x,y) = if x==(-1) then (0,1) else (1,0)
caclNext _ _ = (0,0)

start :: [Text] -> (Int,Int)
start w = (x,y)
    where
        x = fromJust $ D.findIndex (\z-> T.elem 'S' z) w
        y = fromJust $ T.findIndex (=='S') $ w !! x

-- Grid, where am I, where I come from -> resulting grid       
iter :: [Text] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
iter p x y
    | c=='S' = [x]
    | otherwise = x : iter p next x
    where
        c = gchar p x
        next = add x $ calcNext c $ diff x y

shoelace :: [(Int,Int)] -> Int
shoelace ((x,y):(i,j):xs) = (x*j)-(i*y) + (shoelace ((i,j):xs))
shoelace _ = 0

doAlgo :: [Text] -> Int
doAlgo p = (a-l+3) `div` 2
    where
        a = abs (shoelace path)
        l = L.length path
        path = (s) : (iter p new s)
        s = start p
        new = lookaround p s


solve :: Text -> Int
solve x = doAlgo $ T.lines x