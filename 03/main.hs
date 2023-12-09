import Data.Char
import Data.Text.IO
import Data.Text as T
import GHC.List as L
import GHC.Base as B

isSym :: Char -> Bool
isSym a
        | isDigit a = False
        | a=='.' = False
        | otherwise = True

isGear :: Char -> Bool
isGear g
        | g=='*' = True
        | otherwise = False

parseNum :: Int -> Text -> Int
parseNum y x
        | y==0 = read (T.unpack (T.takeWhile (isDigit) (T.drop y x)))
        | isDigit (T.index x (y-1)) = parseNum (y-1) x
        | otherwise = read (T.unpack (T.takeWhile (isDigit) (T.drop y x)))

getChars :: Int -> Text -> Text
getChars x z
        | x==0 = T.take 2 z
        | (x-1) == T.length z = takeEnd 2 z
        | otherwise = T.take 3 (T.drop (x-1) z)

searchAround :: Int -> Int -> [Text] -> Maybe Char
searchAround x y z
        | x == 0 = find (isSym) ((getChars y (z !! 0)) <> (getChars y (z !! 1)))
        | x+1 == L.length z = find (isSym) ((getChars y (z !! x)) <> (getChars y (z !! (x-1))))
        | otherwise = find (isSym) ((getChars y (z !! x)) <> (getChars y (z !! (x-1))) <> (getChars y (z !! (x+1))))

getValue :: Int -> Int -> [Text] -> Maybe Char -> Int 
getValue x y z (Just c) = parseNum y (z !! x)
getValue _ _ _ _ = 0

doCase :: Int -> Int -> [Text] -> Int
doCase x y z
        | isDigit (T.index (z !! x) y) = getValue x y z (searchAround x y z)
        | otherwise = 0

howMuchSkip :: Int -> Int -> [Text] -> Int
howMuchSkip x y z
        | (searchAround x y z) == Nothing = 1
        | y >= T.length (L.head z) = 0
        | isDigit(T.index (z !! x) y) = (howMuchSkip x (y+1) z) + (if (searchAround x y z)==Nothing then 0 else 1)
        | otherwise = 1

iterate :: Int -> Int -> [Text] -> Int
iterate x y z
        | x == L.length z = 0 
        | y >= T.length (L.head z) = Main.iterate (x+1) 0 z
        | otherwise = (Main.iterate x (y+(howMuchSkip x y z)) z) + doCase x y z

doAlgo :: [Text] -> Int
doAlgo x = Main.iterate 0 0 x

----- PART 2 ------

getNumm :: Int -> Text -> Int
getNumm y z
        | isDigit (T.index z y) = parseNum y z
        | otherwise = 0

iterr :: Int -> Int -> Int -> Int -> [Text] -> [Int]
iterr x y a b z
        | x > a = []
        | y < 0 = iterr x 0 a b z
        | y > b = iterr (x+1) (y-3) a b z
        | otherwise = [(getNumm y (z !! x))] ++ (iterr x (y+1) a b z)

searchAroundv2 :: Int -> Int -> [Text] -> [Int]
searchAroundv2 x y z
        | (x == 0) && (y == 0) = iterr x y 1 1 z
        | (x == 0) && (y+1 == T.length (L.head z)) = iterr x (y-1) 1 y z
        | (x == 0) && (y <= T.length (L.head z)) = iterr x (y-1) 1 (y+1) z
        | (x+1 == L.length z) && y == 0 = iterr (x-1) y x 1 z
        | (x+1 == L.length z) && y+1 == T.length (L.head z) = iterr (x-1) (y-1) x y z
        | (x+1 == L.length z) && y <= T.length (L.head z) = iterr (x-1) (y-1) x (y+1) z
        | (y == 0) = iterr (x-1) (y) (x+1) (y+1) z
        | (y+1 == T.length (L.head z)) = iterr (x-1) (y-1) (x+1) (y) z
        | otherwise = iterr (x-1) (y-1) (x+1) (y+1) z

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (L.filter (/= x) xs)

multi :: [Int] -> Int
multi x
        | 2 == L.length x = L.foldl (*) 1 x
        | otherwise = 0

doCasev2 :: Int -> Int -> [Text] -> Int
doCasev2 x y z
        | isGear (T.index (z !! x) y) = multi (L.filter (/=0) (removeDuplicates (searchAroundv2 x y z)))
        | otherwise = 0

iteratev2 :: Int -> Int -> [Text] -> Int
iteratev2 x y z
        | x == L.length z = 0 
        | y >= T.length (L.head z) = Main.iteratev2 (x+1) 0 z
        | otherwise = (doCasev2 x y z) + (Main.iteratev2 x (y+1) z)

doAlgov2 :: [Text] -> Int
doAlgov2 x = Main.iteratev2 0 0 x

main :: IO()
main = do
     content <- Data.Text.IO.readFile "input.txt"
     print (doAlgov2 (T.lines content))