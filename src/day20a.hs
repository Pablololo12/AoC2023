module Day20a (solve) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe
import Debug.Trace

type Entry = String
data State = ON | OFF deriving (Eq,Ord,Show)
data Signal = HIGH | LOW deriving (Eq,Ord,Show)
type Cable = (Entry,Signal,Entry) -- next gate, signal, origin
type Flops = M.Map Entry ([Entry],State)
type Conjunction = M.Map Entry ([Entry],M.Map Entry Signal)

activateFlops :: Cable -> Flops -> [Cable] -> ([Cable],Flops)
activateFlops (e,s,_) f acum
  | M.member e f = if s==HIGH then (acum,f) else ((zip3 ents (repeat change) (repeat e))++acum,addLow)
  | otherwise = (acum,f)
  where
    (ents,m) = (f M.! e)
    change = if m==ON then LOW else HIGH
    changeS = if m==ON then OFF else ON
    addLow = M.insert e (ents,changeS) f

activateConj :: Cable -> Conjunction -> [Cable] -> ([Cable],Conjunction)
activateConj (e,s,o) c acum
  | M.member e c = if allHigh then ((zip3 rets (repeat LOW) (repeat e))++acum,changeSignal) else ((zip3 rets (repeat HIGH) (repeat e))++acum,changeSignal)
  | otherwise = ((e,s,o):acum,c)
  where
    allHigh = not $ LOW `elem` (snd $ unzip $ M.toList $ snd $ changeSignal M.! e)
    changeSignal = M.insert e (rets,(M.insert o s mapa)) c
    (rets,mapa) = c M.! e

activate :: [Cable] -> Flops -> Conjunction -> (Int,Int) -> ((Int,Int),Flops,Conjunction)
activate [] f c acum = (acum,f,c)
activate e f c (aH,aL) = traceShow (newer,newe) activate (newe) newf newc (aH+countH,aL+countL)
  where
    (newer,newf) = foldl (\(li,fl) w -> activateFlops w fl li) ([],f) e
    (newe,newc) = foldl (\(li,fl) w -> activateConj w fl li) ([],c) newer
    countH = foldl (\a (ax,bx,cx) -> if bx==HIGH then a+1 else a) 0 $ removeDuplicates (newe++newer)
    countL = foldl (\a (ax,bx,cx) -> if bx==LOW then a+1 else a) 0 $ removeDuplicates (newe++newer)

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

solve :: T.Text -> Int
solve x = traceShow(iH,(iL+(length ins)*(1+(length input)))) iH*(iL+(length ins)*(1+(length input)))
  where
    result@((iH,iL),f,c) = foldl (\(ii,ff,cc) w -> activate input ff cc ii) ((0,0),flopsM,conjunctions) ins
    ins = [1..1000]
    input = map (\w -> (w,LOW,"BROADCAST")) open
    flopsM = M.fromList flops
    flops = parseFlops $ T.lines x
    conjunctions = M.fromList $ parserCon (T.lines x) flops
    broadcastline = fromJust $ find (\w->(T.index w 0)=='b') $ T.lines x
    open = map (T.unpack) $ map (T.strip) $ T.split (==',') $ last $ T.split (=='>') broadcastline
