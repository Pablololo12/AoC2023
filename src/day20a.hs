module Day20a (solve) where

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

activateFlops :: Cable -> Flops -> (Int,Int) -> [Cable] -> ([Cable],Flops,(Int,Int))
activateFlops (e,s,o) f iii@(iih,iil) acum
  | M.member e f = if s==HIGH then (acum,f,iii) else ((zip3 ents (repeat change) (repeat e))++acum,addLow,(iih+addh,iil+addl))
  | otherwise = ((e,s,o):acum,f,iii)
  where
    (ents,m) = (f M.! e)
    addh = if change==HIGH then (length ents) else 0
    addl = if change==LOW then (length ents) else 0
    change = if m==ON then LOW else HIGH
    changeS = if m==ON then OFF else ON
    addLow = M.insert e (ents,changeS) f

activateConj :: Cable -> Conjunction -> (Int,Int) -> [Cable] -> ([Cable],Conjunction,(Int,Int))
activateConj cab@(e,s,o) c (iih,iil) acum
  | (M.member e c) && allHigh = ((zip3 rets (repeat LOW) (repeat e))++acum,changeSignal,(iih,(iil+(length rets))))
  | M.member e c = ((zip3 rets (repeat HIGH) (repeat e))++acum,changeSignal,(iih+(length rets),iil))
  | otherwise = ((e,s,o):acum,c,(iih,iil))
  where
    allHigh = not $ LOW `elem` (snd $ unzip $ M.toList $ snd $ changeSignal M.! e)
    changeSignal = M.insert e (rets,(M.insert o s mapa)) c
    (rets,mapa) = c M.! e

activate :: [Cable] -> Flops -> Conjunction -> (Int,Int) -> ((Int,Int),Flops,Conjunction)
activate [] f c acum = (acum,f,c)
activate e f c (aH,aL) = activate (newfilt) newf newc (aH+iihf+iiihf,aL+iilf+iiihl)
  where
    (newer,newf,(iiihf,iiihl)) = foldl (\(li,fl,iii) w -> activateFlops w fl iii li) ([],f,(0,0)) e
    (newe,newc,(iihf,iilf)) = foldl (\(li,fl,iii) w -> activateConj w fl iii li) ([],c,(0,0)) newer
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

solve :: T.Text -> Int
solve x = (iH)*(iL+(length ins)*(1+(length input)))
  where
    result@((iH,iL),f,c) = foldl (\(ii,ff,cc) w -> activate input ff cc ii) ((0,0),flopsM,conjunctions) ins
    ins = [1..1000]
    input = map (\w -> (w,LOW,"BROADCAST")) open
    flopsM = M.fromList flops
    flops = parseFlops $ T.lines x
    conjunctions = M.fromList $ parserCon (T.lines x) flops
    broadcastline = fromJust $ find (\w->(T.index w 0)=='b') $ T.lines x
    open = map (T.unpack) $ map (T.strip) $ T.split (==',') $ last $ T.split (=='>') broadcastline
