module Day22a (solve) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.List
import Debug.Trace

type Pos = (Int,Int,Int)
type Brick = (Pos,Pos)
type ExpBrick = [Pos]

expand :: Brick -> [Pos]
expand ((ax,ay,az),(bx,by,bz)) = zip3 [ax..bx] [ay..by] [az..bz]

expand2d :: Brick -> [(Int,Int)]
expand2d ((ax,ay,az),(bx,by,bz))
  | ay==by && ax==bx = [(ax,ay)]
  | ay==by = zip [ax..bx] (repeat ay)
  | ax==bx = zip (repeat ax) [ay..by]
  | otherwise = traceShow ("PROBLEMS") []

inters :: Brick -> Brick -> Bool
inters a b = (length $ intersect (expand2d a) (expand2d b))>0

-- The ones bellow should be ordered by proximity
searchInterj :: Brick -> [Brick] -> Int
searchInterj b [] = 0
searchInterj b (l@((_,_,_),(_,_,z)):ls)
  | inters b l = z
  | otherwise = searchInterj b ls

dropB :: [Brick] -> [Brick] -> [Brick]
dropB [] l = l
dropB (x@((xx,yy,zz),(xxx,yyy,zzz)):xs) l
  | zz==1 = dropB xs (l++[x]) -- It's already on the floor
  | otherwise = dropB xs $ l++[((xx,yy,(zz-count)),(xxx,yyy,(zzz-count)))]
  where
    count = zz-(min+1)
    min = searchInterj x $ reverse $ sortOn (\((_,_,w),(_,_,ww)) -> ww) l -- Pick the ones bellow

getGraph :: [(Int,Brick)] -> M.Map Int [Int] -> M.Map Int [Int]
getGraph [] m = m
getGraph ((x@(i,b@(_,(_,_,z)))):xs) m = getGraph xs $ M.insert i hits m
  where
    hits = fst $ unzip $ filter (\(ind,br) -> inters b br) $ filter (\(_,((_,_,zz),(_))) -> zz==(z+1)) xs

getPos :: [Int] -> Pos
getPos (a:b:c:_) = (a,b,c)

parse :: [T.Text] -> [Brick]
parse [] = []
parse (x:xs) = (part1,part2):parse xs
  where
    part1 = getPos $ map (read . T.unpack) $ T.split (==',') $ head split
    part2 = getPos $ map (read . T.unpack) $ T.split (==',') $ last split
    split = T.split (=='~') x

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

goThrough :: Int -> Int -> M.Map Int [Int] -> Int
goThrough 0 i _ = i
goThrough a i m
  | allhold = goThrough (a-1) (i+1) m
  | otherwise = goThrough (a-1) i m
  where
    whosup = m M.! a
    others = removeDuplicates $ filter (/=0) $ map (\w -> if w `elem` whosup then w else 0) $ concat $ map (\(_,b) -> b) $ M.toList $ M.delete a m
    allhold = (length others) == (length whosup)

solve :: T.Text -> Int
solve x = goThrough (length dropped) 0 graph
  where
    graph = getGraph (zip [1..] dropped) M.empty
    dropped = sortOn (\((_,_,w),(_,_,ww)) -> ww) $ dropB bricks []
    bricks = sortOn (\((_,_,w),(_,_,ww)) -> w) $ parse $ T.lines x
