module Day16b (solve) where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S

data Dir = N | S | E | W deriving (Eq,Ord,Show)
type Pos = (Int,Int)
type Ray = (Pos,Dir)
type Mapa = [[Char]]

calcD :: Pos -> Dir -> Ray
calcD (a,b) N = ((a-1,b),N)
calcD (a,b) S = ((a+1,b),S)
calcD (a,b) E = ((a,b+1),E)
calcD (a,b) W = ((a,b-1),W)

newDir :: Pos -> Dir -> Char -> [Ray]
newDir p d '-'
  | d==E || d==W = [calcD p d]
  | otherwise = [(calcD p W),(calcD p E)]
newDir p d '|'
  | d==N || d==S = [calcD p d]
  | otherwise = [(calcD p N),(calcD p S)]
newDir p N '/' = [calcD p E]
newDir p S '/' = [calcD p W]
newDir p E '/' = [calcD p N]
newDir p W '/' = [calcD p S]
newDir p N '\\' = [calcD p W]
newDir p S '\\' = [calcD p E]
newDir p E '\\' = [calcD p S]
newDir p W '\\' = [calcD p N]
newDir p d _ = [calcD p d]

valid :: Pos -> Pos -> Bool
valid (a,b) (x,y) = x>=0 && x<a && y>=0 && y<b

step :: Mapa -> Ray -> [Ray]
step m (p,d) = filter (\(p,d) -> valid ((length m),(length $ head m)) p) $ newDir p d (m!!(fst p)!!(snd p))

pathTracer :: Mapa -> S.Set (Ray) -> [Ray] -> Int
pathTracer _ visited [] = length $ S.fromList $ map (fst) $ S.toList visited
pathTracer m visited rays = pathTracer m newvisit tovisit
  where
    newRays = concatMap (step m) rays
    newvisit = S.union visited $ S.fromList newRays
    tovisit = filter (\w -> S.notMember w visited) newRays

getAllsides :: Mapa -> Int
getAllsides m = maximum (top++botton++left++right)
  where
    x = length m
    y = length $ head m
    top = [pathTracer m (S.fromList[((0,j),S)]) [((0,j),S)] | j <- [0..(x-1)]]
    botton = [pathTracer m (S.fromList[(((x-1),j),N)]) [(((x-1),j),N)] | j <- [0..(x-1)]]
    left = [pathTracer m (S.fromList[((j,0),E)]) [((j,0),E)] | j <- [0..(y-1)]]
    right = [pathTracer m (S.fromList[((j,(y-1)),W)]) [((j,(y-1)),W)] | j <- [0..(y-1)]]

solve :: T.Text -> Int
solve x = getAllsides mapa
  where
    mapa = map (T.unpack) $ T.lines x
