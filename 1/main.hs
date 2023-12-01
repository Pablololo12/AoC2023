import Data.Char
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
convert x = checkZ x ++ convert (drop 1 x)

getNum :: String -> Int
getNum [] = 0
getNum (x:xs)
       | isNumber x = digitToInt x
       | True = getNum xs

reduceI :: (Int, Int) -> Int
reduceI (x,y) = x*10+y

doAlgo :: String -> [Int]
doAlgo x = map (reduceI) (zip (map (getNum) (map (convert) (lines x))) (map (getNum) (map (reverse) (map (convert) (lines x)))))

main :: IO()
main = do
     content <- readFile "input.txt"
     print (GHC.List.foldl (+) 0 (doAlgo content))
