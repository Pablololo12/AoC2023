module Day18a (solve) where

import qualified Data.Text as T

data Dir = U | D | R | L deriving (Eq,Ord,Show)
type Entry = (Dir,Int)
type Pos = (Int,Int)

parseDir :: Char -> Dir
parseDir 'U' = U
parseDir 'D' = D
parseDir 'R' = R
parseDir 'L' = L

getNewDir :: Entry -> Pos -> Pos
getNewDir (U,s) (i,j) = (i-s,j)
getNewDir (D,s) (i,j) = (i+s,j)
getNewDir (R,s) (i,j) = (i,j+s)
getNewDir (L,s) (i,j) = (i,j-s)

getCoordinates :: [Entry] -> Pos -> [Pos]
getCoordinates [] _ = []
getCoordinates (e:es) p = new:getCoordinates es new
  where
    new = getNewDir e p

shoelace :: [Pos] -> Int
shoelace ((x,y):(i,j):xs) = (x*j)-(i*y) + (shoelace ((i,j):xs))
shoelace _ = 0

getEntry :: [T.Text] -> Entry
getEntry (a:b:_) = ((parseDir $ T.index a 0),(read $ T.unpack b))

parseT :: [T.Text] -> [Entry]
parseT [] = []
parseT (l:ls) = (getEntry $ T.split (==' ') l):parseT ls

solve :: T.Text -> Int
solve x = ((area-l+3) `div` 2) + l
  where
    area = abs $ shoelace $ boundary
    l = foldl (\a (_,n) -> a+n) 0 entries
    boundary = getCoordinates entries (0,0)
    entries = parseT $ T.lines x
