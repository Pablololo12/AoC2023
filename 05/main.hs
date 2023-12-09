import Data.Char
import Data.Text.IO
import Data.Text as T
import GHC.List as L
import GHC.Base as B

parsn :: Text -> Int
parsn x = read (T.unpack x)

headisdigit :: Text -> Bool
headisdigit x = isDigit (T.head x)

filt :: [Int] -> [Int] -> [Int]
filt [] x = []
filt (x:xs) (a:b:c:_)
    | x<0 = x : filt xs [a,b,c]
    | (x>=b) && (x<(b+c)) = -((x-b)+a) : filt xs [a,b,c]
    | otherwise = x : filt xs [a,b,c]

filterall :: [Int] -> [Text] -> [Int]
filterall x [] = x
filterall x (t:ts) = filterall (filt x n) ts
    where n = B.map (parsn) (T.split (==' ') t)

cases :: [Int] -> [Text] -> [Int]
cases x [] = x
cases x (t:ts)
    | isDigit (T.head t) = cases x ts
    | otherwise = cases (L.map (abs) (filterall x (L.takeWhile (headisdigit) ts))) ts

doAlgo :: [Text] -> [Int]
doAlgo (x:xs) = cases (B.map (parsn) (L.drop 1 (T.split (==' ') x))) xs

filtv2 :: [(Int,Int)] -> [Int] -> [(Int,Int)]
filtv2 [] x = []
filtv2 ((f,s):xs) (a:b:c:_)
    | f<0 = (f,s) : filtv2 xs [a,b,c]
    | (f>=b) && ((b+c)>=(f+s)) = (-((f-b)+a),s) : filtv2 xs [a,b,c]
    | (f>=b) && (f<(b+c)) = [(-((f-b)+a),((b+c)-f)),((b+c),((f+s)-(b+c)))] ++ filtv2 xs [a,b,c]
    | ((f+s)>=b) && ((f+s)<(b+c)) = [(f,(b-f)),(-a,((f+s)-b))] ++ filtv2 xs [a,b,c]
    | (f<b) && ((f+s)>(b+c)) = [(f,(b-f)),((b+c),((f+s)-(b+c))),(-a,c)] ++ filtv2 xs [a,b,c]
    | otherwise = (f,s) : filtv2 xs [a,b,c]

filterallv2 :: [(Int,Int)] -> [Text] -> [(Int,Int)]
filterallv2 x [] = x
filterallv2 x (t:ts) = filterallv2 (filtv2 x n) ts
    where n = B.map (parsn) (T.split (==' ') t)

abspar :: (Int,Int) -> (Int,Int)
abspar (a,b) = (abs a, b)

casesv2 :: [(Int,Int)] -> [Text] -> [(Int,Int)]
casesv2 x [] = x
casesv2 x (t:ts)
    | isDigit (T.head t) = casesv2 x ts
    | otherwise = casesv2 (L.map (abspar) (filterallv2 x (L.takeWhile (headisdigit) ts))) ts

generate :: [Int] -> [(Int,Int)]
generate [] = []
generate (a:b:xs) = (a,b) : (generate xs)

doAlgov2 :: [Text] -> [Int]
doAlgov2 (x:xs) = B.map (fst) (casesv2 j xs)
  where j = generate (B.map (parsn) (L.drop 1 (T.split (==' ') x)))

main :: IO()
main = do
     content <- Data.Text.IO.readFile "input.txt"
     print (L.minimum (doAlgov2 ((L.filter (not . T.null) (T.lines content)))))
