module Day1b (solve) where

import Data.Char
import Data.Text.IO
import GHC.Base as B
import GHC.List as L
import Data.Text as T
import GHC.List

checkZ :: String -> String
checkZ ('o':'n':'e':_) = "1"
checkZ ('t':'w':'o':_) = "2"
checkZ ('t':'h':'r':'e':'e':_) = "3"
checkZ ('f':'o':'u':'r':_) = "4"
checkZ ('f':'i':'v':'e':_) = "5"
checkZ ('s':'i':'x':_) = "6"
checkZ ('s':'e':'v':'e':'n':_) = "7"
checkZ ('e':'i':'g':'h':'t':_) = "8"
checkZ ('n':'i':'n':'e':_) = "9"
checkZ (x:xs)
  | isDigit x = [x]
  | otherwise = ""

convert :: String -> String
convert [] = ""
convert x = checkZ x ++ convert (L.drop 1 x)

getNum :: String -> Int
getNum [] = 0
getNum (x:xs)
       | isNumber x = digitToInt x
       | otherwise = getNum xs

doAlgo :: String -> Int
doAlgo x = j*10 + k
    where j = getNum y
          k = getNum $ L.reverse y
          y = convert x

solve :: Text -> Int
solve x = GHC.List.foldl (+) 0 (B.map (doAlgo . unpack) (T.lines x))

main :: IO()
main = do
    content <- Data.Text.IO.readFile "input.txt"
    print $ solve content
