module Main where

import Data.List as L
import Data.Text
import qualified Data.Text.IO
import Distribution.System

import Day1a
import Day1b
import Day2a
import Day2b
import Day3a
import Day3b
import Day4a
import Day4b
import Day5a
import Day5b
import Day6

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
        (Day2b.solve, "day2.txt","Day2b",74229),
        (Day3a.solve, "day3.txt","Day3a",535351),
        (Day3b.solve, "day3.txt","Day3b",87287096),
        (Day4a.solve, "day4.txt","Day4a",23750),
        (Day4b.solve, "day4.txt","Day4b",13261850),
        (Day5a.solve, "day5.txt","Day5a",551761867),
        (Day5b.solve, "day5.txt","Day5b",57451709),
        (Day6.solve, "day6a.txt","Day6a",5133600),
        (Day6.solve, "day6b.txt","Day6b",40651271)]

run :: (Text->Int, String,String,Int) -> IO ()
run (f,s,d,i) = do
            content <- Data.Text.IO.readFile $ pathJoin ["inputs",s]
            let res = f content
            print $ d ++ " " ++ (show res) ++ " " ++ (show $ res==i)

main :: IO ()
main = mapM_ (run) $ days