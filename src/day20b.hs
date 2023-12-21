module Day20b (solve) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe

type Entry = String
data State = ON | OFF deriving (Eq,Ord,Show)
data Signal = HIGH | LOW deriving (Eq,Ord,Show)
type Cable = (Entry,Signal,Entry) -- next gate, signal, origin
type Flops = M.Map Entry ([Entry],State)
type Conjunction = M.Map Entry ([Entry],M.Map Entry Signal)

activateFlops :: Cable -> Flops -> [Cable] -> ([Cable],Flops)
activateFlops (e,s,o) f acum
  | M.member e f = if s==HIGH then (acum,f) else ((zip3 ents (repeat change) (repeat e))++acum,addLow)
  | otherwise = ((e,s,o):acum,f)
  where
    (ents,m) = (f M.! e)
    change = if m==ON then LOW else HIGH
    changeS = if m==ON then OFF else ON
    addLow = M.insert e (ents,changeS) f

activateConj :: Cable -> Conjunction -> [Cable] -> ([Cable],Conjunction)
activateConj cab@(e,s,o) c acum
  | (M.member e c) && allHigh = ((zip3 rets (repeat LOW) (repeat e))++acum,changeSignal)
  | M.member e c = ((zip3 rets (repeat HIGH) (repeat e))++acum,changeSignal)
  | otherwise = ((e,s,o):acum,c)
  where
    allHigh = not $ LOW `elem` (snd $ unzip $ M.toList $ snd $ changeSignal M.! e)
    changeSignal = M.insert e (rets,(M.insert o s mapa)) c
    (rets,mapa) = c M.! e

activate :: [Cable] -> Entry -> Flops -> Conjunction -> (Bool,Flops,Conjunction)
activate [] end f c = (False,f,c)
activate e end f c
  | any (\(a,b,o) -> o==end && b==HIGH) e = (True,f,c)
  | otherwise = activate (newfilt) end newf newc
  where
    (newer,newf) = foldl (\(li,fl) w -> activateFlops w fl li) ([],f) e
    (newe,newc) = foldl (\(li,fl) w -> activateConj w fl li) ([],c) newer
    newfilt = filter (\(ee,_,_) -> (M.member ee f) || (M.member ee c)) (newe)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

parseEn :: T.Text -> (Entry,[Entry])
parseEn x = (a,b)
  where
    a = T.unpack $ head $ T.split (==' ') x
    b = map (T.unpack) $ map (T.strip) $ T.split (==',') $ last $ T.split (=='>') x

parseFlops :: [T.Text] -> [(Entry,([Entry],State))]
parseFlops [] = []
parseFlops (x:xs)
  | (T.index x 0)=='%' = (a,(b,OFF)):parseFlops xs
  | otherwise = parseFlops xs
  where
    (a,b) = parseEn (T.drop 1 x)

parserCon :: [T.Text] -> [(Entry,([Entry],State))] -> [(Entry,([Entry],M.Map Entry Signal))]
parserCon [] _ = []
parserCon (x:xs) m
  | (T.index x 0)=='&' = (a,(b,zipped)):parserCon xs m
  | otherwise = parserCon xs m
  where
    (a,b) = parseEn (T.drop 1 x)
    zipped = M.fromList $ zip l $ repeat LOW
    l = foldl (\z (f,(g,h)) -> if a `elem` g then f:z else z) [] m

whilefalse :: Int -> Entry -> [Cable] -> Flops -> Conjunction -> Int
whilefalse acum end e f c
  | cierto = acum
  | otherwise = whilefalse (acum+1) end e ff cc
  where
    (cierto,ff,cc) = activate e end f c

lcmm :: [Int] -> Int
lcmm [] = 1
lcmm (x:xs) = lcm x (lcmm xs)

solve :: T.Text -> Int
solve x = lcmm nums
  where
    nums = map (\w -> whilefalse 1 w input flopsM conjunctions) ["xj","qs","kz","km"]
    input = map (\w -> (w,LOW,"BROADCAST")) open
    flopsM = M.fromList flops
    flops = parseFlops $ T.lines x
    conjunctions = M.fromList $ parserCon (T.lines x) flops
    broadcastline = fromJust $ find (\w->(T.index w 0)=='b') $ T.lines x
    open = map (T.unpack) $ map (T.strip) $ T.split (==',') $ last $ T.split (=='>') broadcastline
