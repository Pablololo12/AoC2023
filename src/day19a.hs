module Day19a (solve) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Debug.Trace

data Var = X | M | A | S | Z deriving (Eq,Ord,Show)
type Entry = (Var,(Int->Int->Bool),Int,String)
type Input = M.Map Var Int

noop :: Int -> Int -> Bool
noop _ _ = True

parseVar :: Char -> Var
parseVar 'x' = X
parseVar 'm' = M
parseVar 'a' = A
parseVar 's' = S

parseEntry :: T.Text -> Entry
parseEntry x
  | (not (':' `T.elem` x)) = (Z,noop,0,(T.unpack x))
  | gt = ((parseVar (T.index (head col) 0)),(>),(read $ T.unpack $ last operation),(T.unpack $ last col))
  | lt = ((parseVar (T.index (head col) 0)),(<),(read $ T.unpack $ last operation),(T.unpack $ last col))
  | otherwise = (Z,noop,0,(T.unpack x))
  where
    col = T.split (==':') x
    gt = '>' `T.elem` x
    lt = '<' `T.elem` x
    operation = T.split (\w -> w=='<'||w=='>') $ head col

parseRules :: [T.Text] -> [(String,[Entry])]
parseRules [] = []
parseRules (x:xs) = (indx,rules):parseRules xs
  where
    indx = T.unpack $ head sp1
    rules = map (parseEntry) $ T.split (==',') sp2
    sp1 = T.split (=='{') x
    sp2 = head $ T.split (=='}') $ last sp1

getIn :: T.Text -> (Var,Int)
getIn x = (a,b)
  where
    a = parseVar $ T.index x 0
    b = read $ T.unpack $ last $ T.split (=='=') x

parseInputs :: [T.Text] -> [Input]
parseInputs [] = []
parseInputs (x:xs) = (M.fromList $ map (getIn) clean):parseInputs xs
  where
    clean = T.split (==',') $ T.drop 1 $ T.dropEnd 1 x

algoAux :: [Entry] -> Input -> String
algoAux (x@(a,f,b,c):xs) i = if (f (i M.! a) b) then c else algoAux xs i

algo :: M.Map String [Entry] -> String -> Input -> Bool
algo _ "A" _ = True
algo _ "R" _ = False
algo ent ind inp = algo ent getN inp
  where
    ents = ent M.! ind
    getN = algoAux ents inp

sumEnt :: Input -> Int
sumEnt i = (i M.! X)+(i M.! M)+(i M.! A)+(i M.! S)

solve :: T.Text -> Int
solve x = traceShow (accept) foldl (\a (c,d) -> if d then a+c else a) 0 $ zip count accept
  where
    count = map (sumEnt) inputs
    accept = map (\w -> algo rules "in" w) inputs
    rules = M.fromList $ parseRules $ fst lines
    inputs = parseInputs $ tail $ snd lines
    lines = splitAt wheretosplit $ T.lines x
    wheretosplit = fromJust $ elemIndex (T.pack "") $ T.lines x
