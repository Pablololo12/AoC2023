module Day17a (solve) where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Algorithm.Search as Dij
import Data.Char
import Data.Maybe
import Debug.Trace

data Dir = N | S | E | W deriving (Eq,Ord,Show)
type Pos = (Int,Int)
type Point = (Pos,Dir,Int) -- Position, direction, count
type Cost = (Int, Pos) -- We save cost until node and the previous path
type Mapa = [[Int]]

newDirs :: Point -> [Point]
newDirs ((a,b),N,n) = [((a-1, b),N,(n+1)),((a,b-1),W,1),((a,b+1),E,1)]
newDirs ((a,b),S,n) = [((a+1, b),S,(n+1)),((a,b-1),W,1),((a,b+1),E,1)]
newDirs ((a,b),E,n) = [((a-1, b),N,1),((a+1,b),S,1),((a,b+1),E,(n+1))]
newDirs ((a,b),W,n) = [((a-1, b),N,1),((a+1,b),S,1),((a,b-1),W,(n+1))]

neighbours :: Mapa -> Point -> [(Point,Int)]
neighbours m p = map (\w@((a,b),_,n) -> (w,(m!!a)!!b)) valids
  where
    valids = filter (\((a,b),_,n) -> a>=0 && a<u && b>=0 && b<v && n<=3) $ newDirs p
    u = length m
    v = length $ head m

final :: Mapa -> Point -> Bool
final m ((a,b),_,_) = ((length m)-1)==a && ((length $ head m)-1)==b

getMapa :: [T.Text] -> Mapa
getMapa [] = []
getMapa (a:as) = [(map (digitToInt) $ T.unpack a)] ++ getMapa as

solve :: T.Text -> Int
solve x = minimum $ map (\w -> fst $ fromJust w) d
  where
    d = map (\w -> Dij.dijkstraAssoc (neighbours mapa) (final mapa) w) start
    mapa = getMapa $ T.lines x
    start = [((0,0),E,1),((0,0),S,1)]
