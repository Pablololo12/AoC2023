import Data.Char
import Data.Text.IO
import Data.Text as T
import GHC.List as L
import GHC.Base as B

data Game = Game {red :: Int, green :: Int, blue :: Int}

folder :: Int -> Int -> Int
folder 1 0 = 0
folder 1 1 = 1
folder 0 _ = 0
folder _ _ = -1

getID :: Text -> Int
getID x = read (T.unpack (L.last (T.split (==' ') (L.head (T.split (==':') x)))))

check :: Int -> String -> Int
check x "red" = if x <= 12 then 1 else 0
check x "green" = if x <= 13 then 1 else 0
check x "blue" = if x <= 14 then 1 else 0
check _ _ = -1

checkC :: Text -> Int
checkC x = check (read (T.unpack (L.head (T.split (==' ') x)))) (T.unpack (L.last (T.split (==' ') x)))

checkCase :: Text -> Int
checkCase x = L.foldl (folder) 1 (B.map (checkC) (B.map (T.strip) (T.split (==',') x)))

checkCases :: Text -> Int
checkCases x = (getID x) * (L.foldl (folder) 1 (B.map (checkCase) (T.split (==';') (L.last (T.split (==':') x)))))

doAlgo :: [Text] -> [Int]
doAlgo x = B.map (checkCases) x

maxGame :: Game -> Game -> Game
maxGame a b = Game {red=if (red a)>(red b) then (red a) else (red b),
                    green=if (green a)>(green b) then (green a) else (green b),
                    blue=if (blue a)>(blue b) then (blue a) else (blue b)}

cRed :: Int -> String -> Int
cRed x "red" = x
cRed _ _ = 0

getRed :: [Text] -> Int
getRed (x:xs) = (cRed (read (T.unpack (L.head (T.split (==' ') x)))) (T.unpack (L.last (T.split (==' ') x)))) + (getRed xs)
getRed [] = 0

cGreen :: Int -> String -> Int
cGreen x "green" = x
cGreen _ _ = 0

getGreen :: [Text] -> Int
getGreen (x:xs) = (cGreen (read (T.unpack (L.head (T.split (==' ') x)))) (T.unpack (L.last (T.split (==' ') x)))) + (getGreen xs)
getGreen [] = 0

cBlue :: Int -> String -> Int
cBlue x "blue" = x
cBlue _ _ = 0

getBlue :: [Text] -> Int
getBlue (x:xs) = (cBlue (read (T.unpack (L.head (T.split (==' ') x)))) (T.unpack (L.last (T.split (==' ') x)))) + (getBlue xs)
getBlue [] = 0

checkCv2 :: [Text] -> Game
checkCv2 x = Game {red=(getRed x), green=(getGreen x), blue=(getBlue x)}

checkCasev2 :: Text -> Game
checkCasev2 x = checkCv2 (B.map (T.strip) (T.split (==',') x))

mul :: Game -> Int
mul g = (red g) * (green g) * (blue g)

checkCasesv2 :: Text -> Int
checkCasesv2 x = mul (L.foldl (maxGame) Game{red=0,green=0,blue=0} (B.map (checkCasev2) (T.split (==';') (L.last (T.split (==':') x)))))

doAlgov2 :: [Text] -> [Int]
doAlgov2 x = B.map (checkCasesv2) x

main :: IO()
main = do
     content <- Data.Text.IO.readFile "input.txt"
     print (L.foldl (+) 0 (doAlgov2 (T.lines content)))
