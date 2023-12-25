module Day22b (solve) where

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
  | ay==by = zip [ax..bx] $ repeat ay
  | ax==bx = zip (repeat ax) [ay..by]
  | otherwise = traceShow ("PROBLEMS") []

inters :: Brick -> Brick -> Bool
inters a b = (length $ intersect (expand2d a) (expand2d b))>0

-- The ones bellow should be ordered by proximity
searchInterj :: Brick -> [Brick] -> Int
searchInterj _ [] = 0
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

getPos :: [Int] -> Pos
getPos (a:b:c:_) = (a,b,c)

parse :: [T.Text] -> [Brick]
parse [] = []
parse (x:xs) = (part1,part2):parse xs
  where
    part1 = getPos $ map (read . T.unpack) $ T.split (==',') $ head split
    part2 = getPos $ map (read . T.unpack) $ T.split (==',') $ last split
    split = T.split (=='~') x

simulate :: Int -> [Brick] -> Int -> Int
simulate 0 _ a = a
simulate i b a
 | (length $ (b!!(i-1))) == 0 = simulate (i-1) b a
 | otherwise = simulate (i-1) b (a+count)
  where
    btosim = sortOn (\((_,_,w),(_,_,ww)) -> w) $ delete (b !! (i-1)) b
    sims = dropB btosim []
    elementssame = intersect sims btosim
    count = (length btosim)-(length elementssame)

getGraph :: [(Int,Brick)] -> M.Map Int [Int] -> M.Map Int [Int]
getGraph [] m = m
getGraph ((x@(i,b@(_,(_,_,z)))):xs) m = getGraph xs $ M.insert i hits m
  where
    hits = fst $ unzip $ filter (\(ind,br) -> inters b br) $ filter (\(_,((_,_,zz),(_))) -> zz==(z+1)) xs

reverseGraph :: [(Int,[Int])] -> Int -> [(Int,[Int])]
reverseGraph _ 0 = []
reverseGraph m i = (i,whos):reverseGraph m (i-1)
  where
    whos = foldl (\l (w,ws) -> if i `elem` ws then w:l else l) [] m

remv :: [(Int,[Int])] -> [Int] -> Int
remv l i
  | (length i) == (length $ removeDuplicates (i++toremove)) = (length i)-1
  | otherwise = remv l (removeDuplicates (i++toremove))
  where
    toremove = foldl (\ls (w,ws) -> if null (filter (\a -> not (a `elem` i)) ws) then w:ls else ls) [] l

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

solve :: T.Text -> Int
solve x = foldl (\a w -> a+remv rev [w]) 0 [1..(length bricks)]
  where
    rev = map (\(ll,l) -> if null l then (ll,[0]) else (ll,l)) $ reverseGraph (M.toList graph) (length dropped)
    graph = getGraph (zip [1..] dropped) M.empty
    dropped = sortOn (\((_,_,w),(_,_,ww)) -> ww) $ dropB bricks []
    bricks = sortOn (\((_,_,w),(_,_,ww)) -> w) $ parse $ T.lines x
