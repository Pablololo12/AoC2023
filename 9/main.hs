import Data.Char
import Data.Text.IO
import Data.Text as T
import Data.List as D
import GHC.List as L
import GHC.Base as B
import Data.Map as M
import Debug.Trace

reduce :: [Int] -> [Int]
reduce (x:y:xs) = (y-x) : reduce (y:xs) 
reduce _ = []

iter :: [Int] -> Int
iter x
    | z = (L.last x)
    | otherwise = (L.last x) + (iter r)
    where z = L.all (==0) r
          r = reduce x

extrapolate :: [[Int]] -> [Int]
extrapolate x = j
    where j = B.map (iter) x

doAlgo :: [Text] -> Int
doAlgo x = L.foldl (+) 0 $ extrapolate j
    where j = B.map (\w -> B.map (read . unpack) w) (B.map (T.words) x)

doAlgov2 :: [Text] -> Int
doAlgov2 x = L.foldl (+) 0 $ extrapolate j
    where j = B.map (L.reverse) $ B.map (\w -> B.map (read . unpack) w) (B.map (T.words) x)

main :: IO()
main = do
     content <- Data.Text.IO.readFile "input.txt"
     print (doAlgov2 (T.lines content))