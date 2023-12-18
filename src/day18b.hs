module Day18b (solve) where

import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Char

data Dir = U | D | R | L deriving (Eq,Ord,Show)
type Entry = (Dir,Int)
type Pos = (Int,Int)

toDig :: Char -> Int
toDig c = fromJust $ elemIndex c "0123456789abcdef"

parseDir :: Char -> Dir
parseDir '3' = U
parseDir '1' = D
parseDir '0' = R
parseDir '2' = L

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

getHex :: [Char] -> Int
getHex [] = 0
getHex x = toDig (last x) + 16 * getHex (init x)

getEntry :: T.Text -> Entry
getEntry x = ((parseDir $ head $ T.unpack $ T.takeEnd 1 x),(getHex (T.unpack $ T.take ((T.length x)-1) x)))

parseT :: [T.Text] -> [Entry]
parseT [] = []
parseT (l:ls) = (getEntry $ T.filter (\w -> w/='(' && w/='#' && w/=')') $ last $ T.split (==' ') l):parseT ls

solve :: T.Text -> Int
solve x = ((area-l+3) `div` 2) + l
  where
    area = abs $ shoelace $ boundary
    l = foldl (\a (_,n) -> a+n) 0 entries
    boundary = getCoordinates entries (0,0)
    entries = parseT $ T.lines x
