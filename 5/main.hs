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

generate :: [Int] -> [Int]
generate [] = []
generate (a:b:xs) = (L.take b [a..]) ++ (generate xs)

doAlgov2 :: [Text] -> [Int]
doAlgov2 (x:xs) = cases (generate (B.map (parsn) (L.drop 1 (T.split (==' ') x)))) xs

main :: IO()
main = do
     content <- Data.Text.IO.readFile "input.txt"
     print (L.minimum (doAlgov2 ((L.filter (not . T.null) (T.lines content)))))
