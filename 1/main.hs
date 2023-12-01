import Data.Char
import GHC.List

getNum :: String -> Int
getNum [] = 0
getNum (x:xs)
       | isNumber x = digitToInt x
       | True = getNum xs

checkOne :: String -> String
checkOne (x:y:_) = if x == 'n' && y == 'e' then "1"
                   else "z"
checkOne _ = "z"

checkThree :: String -> String
checkThree (x:y:_) = if x == 'e' && y == 'e' then "3"
                   else "z"
checkThree _ = "z"

checkT :: String -> String
checkT (x:y:xs) = if x == 'w' && y == 'o' then "2"
                 else if x == 'h' && y == 'r' then checkThree xs
                 else "z"
checkT _ = "z"

checkF :: String -> String
checkF (x:y:z:_) = if x == 'o' && y == 'u' && z == 'r' then "4"
                 else if x == 'i' && y == 'v' && z == 'e' then "5"
                 else "z"
checkF _ = "z"

checkSeven :: String -> String
checkSeven (x:y:_) = if x == 'e' && y == 'n' then "7"
                   else "z"
checkSeven _ = "z"

checkS :: String -> String
checkS (x:y:xs) = if x == 'i' && y == 'x' then "6"
                 else if x == 'e' && y == 'v' then checkSeven xs
                 else "z"
checkS _ = "z"

checkEight :: String -> String
checkEight (x:y:z:w:_) = if x == 'i' && y == 'g' && z == 'h' && w == 't' then "8"
                   else "z"
checkEight _ = "z"

checkNine :: String -> String
checkNine (x:y:z:_) = if x == 'i' && y == 'n' && z == 'e' then "9"
                   else "z"
checkNine _ = "z"

-- one two three four five six seven eight nine
checkN :: String -> String
checkN [] = "z"
checkN (x:xs) = case () of
       () | x == 'o' -> checkOne xs
          | x == 't' -> checkT xs
          | x == 'f' -> checkF xs
          | x == 's' -> checkS xs
          | x == 'e' -> checkEight xs
          | x == 'n' -> checkNine xs
          | isDigit x -> [x]
          | otherwise -> "z"

convert :: String -> String
convert [] = ""
convert x = checkN x ++ convert (drop 1 x)

reduceI :: (Int, Int) -> Int
reduceI (x,y) = x*10+y

doAlgo :: String -> [Int]
doAlgo x = map (reduceI) (zip (map (getNum) (map (convert) (lines x))) (map (getNum) (map (reverse) (map (convert) (lines x)))))

main :: IO()
main = do
     content <- readFile "input.txt"
     print (GHC.List.foldl (+) 0 (doAlgo content))
     --print (convert content)