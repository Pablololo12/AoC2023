module Day17a (solve) where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Char
import Debug.Trace

Data Dir = N | S | E | W deriving (Eq,Ord,Show)
type Pos = (Int,Int)
type Point = (Pos,Dir,Int) -- Position, direction, count
type Cost = (Int, Pos) -- We save cost until node and the previous path
type Mapa = [[Int]]

newDirs :: Point -> [Points]
newDirs ((a,b),N,n) -> [((a-1, b),N,(n+1)),((a,b-1),W,1),((a,b+1),E,1)]
newDirs ((a,b),S,n) -> [((a+1, b),S,(n+1)),((a,b-1),W,1),((a,b+1),E,1)]
newDirs ((a,b),E,n) -> [((a-1, b),N,1)),((a+1,b),S,1),((a,b+1),E,(n+1))]
newDirs ((a,b),W,n) -> [((a-1, b),N,1)),((a+1,b),S,1),((a,b-1),W,(n+1))]

dijkstra :: S.Set(Pos) -> M.Map Point Cost -> Mapa -> M.Map Point Cost
dijkstra open scores m
  | S.null open = scores
  | otherwise = dijkstra newopen m
  where
    newScore = foldl (\s w -> visitNeighbor m s best w) score $ filter (\(_,_,n) -> n<=3) $ filter (\(x,_,_) -> S.member x newopen) $ newDirs best
    newopen = S.delete best open
    best = fst $ L.minimumBy (\(_,(a,_)) (_,(b,_)) -> compare a b) $ L.filter (\(w,i) -> S.member w open) (M.toList score)

visitNeighbor :: Mapa -> M.Map Pos Cost -> Pos -> Pos -> M.Map Pos Cost
visitNeighbor m score (a,b) (i,j)
  | i<0 || j<0 || i>=(length m) || j>=(length $ head m) = score
  | (sameH || sameV) && (length (fst prev))>=4 = score
  | otherwise = if alt<(fst (score M.! (i,j))) then (M.insert (i,j) (alt,(a,b)) score) else score
  where
    alt = (fst (score M.! (a,b))) + ((m!!i)!!j)
    prev = unzip $ [(i,j)]++(take 3 $ reversePath (a,b) score)
    sameH = all (==(head (snd prev))) (snd prev)
    sameV = all (==(head (fst prev))) (fst prev)

getDirs :: Pos -> [Pos]
getDirs (a,b) = [((a-1),b),((a+1),b),(a,(b-1)),(a,(b+1))]

-- Open set of nodes searching, cheapest path, prevPath, map
pathTracer :: S.Set (Pos) -> M.Map Pos Cost -> Mapa -> M.Map Pos Cost
pathTracer open score m
  | S.null open = score
  | otherwise = pathTracer newopen newScore m
  where
    newScore = foldl (\s w -> visitNeighbor m s best w) score $ filter (`S.member` newopen) $ getDirs best
    u = score M.! best
    newopen = S.delete best open
    best = fst $ L.minimumBy (\(_,(a,_)) (_,(b,_)) -> compare a b) $ L.filter (\(w,i) -> S.member w open) (M.toList score)

doAlgo :: Mapa -> M.Map Pos Cost
doAlgo m = pathTracer set (M.insert (0,0) (((m!!0)!!0),(0,0)) ma) m
  where
    set = S.fromList coordinates
    ma = M.fromList $ zip coordinates $ repeat (999999,(-1,-1))
    coordinates = [(i,j) | i <- [0..(x-1)], j <- [0..(y-1)]]
    x = length m
    y = length $ head m

reversePath :: Pos -> M.Map Pos Cost -> [Pos]
reversePath (x,y) m
  | x==0 && y==0 = [(0,0)]
  | otherwise = (x,y):(reversePath (snd (m M.! (x,y))) m)

calculateLoss :: [Pos] -> Mapa -> Int
calculateLoss x m = foldl (\acum (a,b) -> acum + (m!!a)!!b) 0 x

getMapa :: [T.Text] -> Mapa
getMapa [] = []
getMapa (a:as) = [(map (digitToInt) $ T.unpack a)] ++ getMapa as

solve :: T.Text -> Int
solve x = traceShow (path) calculateLoss path mapa
  where
    path = reversePath (((length mapa)-1),((length $ head mapa)-1)) paths
--    path = reversePath (0,0) paths
    paths = doAlgo mapa
    mapa = getMapa $ T.lines x
