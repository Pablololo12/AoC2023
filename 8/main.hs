import Data.Char
import Data.Text.IO
import Data.Text as T
import Data.List as D
import GHC.List as L
import GHC.Base as B
import Data.Map as M
import Debug.Trace

-- List of instructions, Rest of instructions, accumulator, next step, map of steps -> result
iter :: [Int] -> [Int] -> Int -> Int -> [(Int,Int)] -> Int
iter f [] c w m = iter f f c w m
iter f (x:xs) c w m
    | w == -1 = c
    | x == 0 = iter f xs (c+1) (fst (m !! w)) m
    | x == 1 = iter f xs (c+1) (snd (m !! w)) m

maptoint :: String -> [Int]
maptoint [] = []
maptoint (x:xs) = j : maptoint xs
    where j = if x=='R' then 1 else 0

prepLines :: [Text] -> Map Text Int -> [(Int, Int)]
prepLines [] _ = []
prepLines (x:xs) m = (m ! (L.head b), m ! (L.last b)) : prepLines xs m
    where b = T.split (==',') (T.filter (\w -> ((w/='(') && (w/=')'))) (L.last c))
          c = T.split (=='=') (T.filter (/=' ') x)

doAlgo :: [Text] -> Int
doAlgo (x:xs) = iter j j 0 (n ! (pack "AAA")) w
    where j = maptoint (unpack x)
          w = prepLines xs n
          n = fromList ((L.filter (\w -> (fst w)/=(pack "ZZZ")) (L.zip  m [0..])) ++ [((pack "ZZZ"), -1)])
          m = B.map (T.filter (/=' ')) (B.map (L.head . (T.split (=='='))) xs)

main :: IO()
main = do
     content <- Data.Text.IO.readFile "input.txt"
     print (doAlgo (L.filter (not . T.null) (T.lines content)))
