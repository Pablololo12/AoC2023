import Data.Char
import Data.Text.IO
import Data.Text as T
import Data.List as D
import GHC.List as L
import GHC.Base as B
import Data.Map as M
import Debug.Trace

iter :: [Int] -> [Int] -> Int -> Text -> Map Text (Text,Text) -> Int
iter f [] c w m = iter f f c w m
iter f (x:xs) c w m
    | w == pack("ZZZ") = c
    | x == 0 = iter f xs (c+1) (fst (m ! w)) m
    | x == 1 = iter f xs (c+1) (snd (m ! w)) m

iterv3 :: [Int] -> [Int] -> Int -> Text -> Map Text (Text,Text) -> [Int]
iterv3 f [] c w m = iterv3 f f c w m
iterv3 f (x:xs) c w m
    | (T.last w)=='Z' && x==0 = c : (iterv3 f xs (c+1) (fst (m ! w)) m)
    | (T.last w)=='Z' && x==1 = c : (iterv3 f xs (c+1) (snd (m ! w)) m)
    | x == 0 = (iterv3 f xs (c+1) (fst (m ! w)) m)
    | x == 1 = (iterv3 f xs (c+1) (snd (m ! w)) m)

allFinishZ :: [Text] -> Bool
allFinishZ [] = True
allFinishZ (x:xs)
    | (T.last x)=='Z' = allFinishZ xs
    | otherwise = False

applyMoves :: Int -> [Text] -> Map Text (Text, Text) -> [Text] -> [Text]
applyMoves _ [] _ x = x
applyMoves 0 (x:xs) m y = applyMoves 0 xs m (y++[(fst (m ! x))])
applyMoves 1 (x:xs) m y = applyMoves 1 xs m (y++[(snd (m ! x))])

-- Instructions, rest of inst, count, curIndx, Map -> final value
iterv2 :: [Int] -> [Int] -> Int -> [Text] -> Map Text (Text,Text) -> Int
iterv2 f [] c w m = iterv2 f f c w m
iterv2 f (x:xs) c w m
    | allFinishZ w = c
    | otherwise = iterv2 f xs (c+1) (applyMoves x w m []) m

maptoint :: String -> [Int]
maptoint [] = []
maptoint (x:xs) = j : maptoint xs
    where j = if x=='R' then 1 else 0

prepLines :: [Text] -> [(Text, (Text, Text))]
prepLines [] = []
prepLines (x:xs) = (a,(L.head b, L.last b)) : prepLines xs
    where a = L.head c
          b = T.split (==',') (T.filter (\w -> ((w/='(') && (w/=')'))) (L.last c))
          c = T.split (=='=') (T.filter (/=' ') x)

doAlgo :: [Text] -> Int
doAlgo (x:xs) = iter j j 0 (pack "AAA") w
    where j = maptoint (unpack x)
          w = fromList (prepLines xs)

doAlgov2 :: [Text] -> Int
doAlgov2 (x:xs) = iterv2 j j 0 m w
    where j = maptoint (unpack x)
          m = L.filter (\y -> (T.last y)=='A') (B.map (T.filter (/=' ')) (B.map (L.head . (T.split (=='='))) xs))
          w = fromList (prepLines xs)

search :: Int -> [Int] -> Bool
search _ [] = False
search a (x:xs)
        | a<x = False
        | a==x = True
        | otherwise = search a xs

reduce :: Int -> [[Int]] -> Bool
reduce _ [] = True
reduce a (x:xs)
        | search a x = reduce a xs
        | otherwise = False

redd :: [Int] -> [[Int]] -> Int
redd [] _ = 0
redd (x:xs) a
        | reduce x a = x
        | otherwise = redd xs (B.map (L.dropWhile(<x)) a)

red :: [[Int]] -> Int
red [] = 0
red (x:xs) = redd x xs

doAlgov3 :: [Text] -> Int
doAlgov3 (x:xs) = red (B.map (\y -> iterv3 j j 0 y w) m)
    where j = maptoint (unpack x)
          m = L.filter (\y -> (T.last y)=='A') (B.map (T.filter (/=' ')) (B.map (L.head . (T.split (=='='))) xs))
          w = fromList (prepLines xs)

main :: IO()
main = do
     content <- Data.Text.IO.readFile "input.txt"
     print (doAlgov3 (L.filter (not . T.null) (T.lines content)))
