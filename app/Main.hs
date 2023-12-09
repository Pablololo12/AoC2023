module Main where

import Data.List as L
import Data.Text
import qualified Data.Text.IO
import Distribution.System

import Day1a
import Day1b
import Day2a
import Day2b

retdiv :: OS -> String
retdiv Windows = "\\"
retdiv _ = "/"

pathJoin :: [String] -> String
pathJoin x = L.intercalate j x
    where j = retdiv buildOS

days :: [(Text->Int,String,String,Int)]
days = [(Day1a.solve, "day1.txt","Day1a",54630),
        (Day1b.solve, "day1.txt","Day1b",54770),
        (Day2a.solve, "day2.txt","Day2a",2256),
        (Day2b.solve, "day2.txt","Day2b",74229)]

run :: (Text->Int, String,String,Int) -> IO ()
run (f,s,d,i) = do
            content <- Data.Text.IO.readFile $ pathJoin ["inputs",s]
            let res = f content
            print $ d ++ " " ++ (show res) ++ " " ++ (show $ res==i)

main :: IO ()
main = mapM_ (run) $ days