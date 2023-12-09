module Day3a (solve) where

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
        | y >= T.length (L.head z) = Day3a.iterate (x+1) 0 z
        | otherwise = (Day3a.iterate x (y+(howMuchSkip x y z)) z) + doCase x y z

doAlgo :: [Text] -> Int
doAlgo x = Day3a.iterate 0 0 x

solve :: Text -> Int
solve x = doAlgo $ T.lines x