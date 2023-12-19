module Day19b (solve) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Debug.Trace

data Var = X | M | A | S | Z deriving (Eq,Ord,Show)
data Op = GGT | LLT | T deriving (Eq,Ord,Show)
type Entry = (Var,Op,Int,String)
type Input = M.Map Var [Int]

parseVar :: Char -> Var
parseVar 'x' = X
parseVar 'm' = M
parseVar 'a' = A
parseVar 's' = S

parseEntry :: T.Text -> Entry
parseEntry x
  | (not (':' `T.elem` x)) = (Z,T,0,(T.unpack x))
  | gt = ((parseVar (T.index (head col) 0)),GGT,(read $ T.unpack $ last operation),(T.unpack $ last col))
  | lt = ((parseVar (T.index (head col) 0)),LLT,(read $ T.unpack $ last operation),(T.unpack $ last col))
  | otherwise = (Z,T,0,(T.unpack x))
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

-- Done parsing

algoAux :: [Entry] -> Input -> [(String,Input)]
algoAux (x@(a,f,b,c):xs) i
  | f==GGT && (head (i M.! a))>b = [(c,i)]
  | f==LLT && (last (i M.! a))<b = [(c,i)]
  | f==GGT && (last (i M.! a))<b = algoAux xs i
  | f==LLT && (head (i M.! a))>b = algoAux xs i
  | f==GGT = (c, (M.insert a (dropWhile (<=b) (i M.! a)) i)):algoAux xs (M.insert a (takeWhile (<b) (i M.! a)) i)
  | f==LLT = (c, (M.insert a (takeWhile (<b) (i M.! a)) i)):algoAux xs (M.insert a (dropWhile (<=b) (i M.! a)) i)
  | otherwise = [(c,i)]

sumEnt :: Input -> Int
sumEnt i = (length (i M.! X))*(length (i M.! M))*(length (i M.! A))*(length (i M.! S))

algo :: M.Map String [Entry] -> String -> Input -> Int
algo _ "A" i = sumEnt i
algo _ "R" _ = 0
algo ent ind inp = foldl (\a (s,e) -> a + (algo ent s e)) 0 getN
  where
    ents = ent M.! ind
    getN = algoAux ents inp

solve :: T.Text -> Int
solve x = accept
  where
    accept = algo rules "in" $ M.fromList [(X,[1..4000]),(M,[1..4000]),(A,[1..4000]),(S,[1..4000])]
    rules = M.fromList $ parseRules $ fst lines
    lines = splitAt wheretosplit $ T.lines x
    wheretosplit = fromJust $ elemIndex (T.pack "") $ T.lines x
