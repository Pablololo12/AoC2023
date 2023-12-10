module Main where

import Data.List as L
import Data.Text
import Data.Maybe
import qualified Data.Text.IO
import Distribution.System
import System.Environment

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
import Day7a
import Day7b
import Day8a
import Day8b
import Day9a
import Day9b
import Day10a
import Day10b
import Day11a
import Day11b
import Day12a
import Day12b
import Day13a
import Day13b
import Day14a
import Day14b
import Day15a
import Day15b
import Day16a
import Day16b
import Day17a
import Day17b
import Day18a
import Day18b
import Day19a
import Day19b
import Day20a
import Day20b
import Day21a
import Day21b
import Day22a
import Day22b
import Day23a
import Day23b
import Day24a
import Day24b
import Day25a
import Day25b

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
        (Day6.solve, "day6b.txt","Day6b",40651271),
        (Day7a.solve, "day7.txt","Day7a",253954294),
        (Day7b.solve, "day7.txt","Day7b",254837398),
        (Day8a.solve, "day8.txt","Day8a",16579),
        (Day8b.solve, "day8.txt","Day8b",12927600769609),
        (Day9a.solve, "day9.txt","Day9a",1853145119),
        (Day9b.solve, "day9.txt","Day9b",923),
        (Day10a.solve, "day10.txt","Day10a",6786),
        (Day10b.solve, "day10.txt","Day10b",495),
        (Day11a.solve, "day11.txt","Day11a",0),
        (Day11b.solve, "day11.txt","Day11b",0),
        (Day12a.solve, "day12.txt","Day12a",0),
        (Day12b.solve, "day12.txt","Day12b",0),
        (Day13a.solve, "day13.txt","Day13a",0),
        (Day13b.solve, "day13.txt","Day13b",0),
        (Day14a.solve, "day14.txt","Day14a",0),
        (Day14b.solve, "day14.txt","Day14b",0),
        (Day15a.solve, "day15.txt","Day15a",0),
        (Day15b.solve, "day15.txt","Day15b",0),
        (Day16a.solve, "day16.txt","Day16a",0),
        (Day16b.solve, "day16.txt","Day16b",0),
        (Day17a.solve, "day17.txt","Day17a",0),
        (Day17b.solve, "day17.txt","Day17b",0),
        (Day18a.solve, "day18.txt","Day18a",0),
        (Day18b.solve, "day18.txt","Day18b",0),
        (Day19a.solve, "day19.txt","Day19a",0),
        (Day19b.solve, "day19.txt","Day19b",0),
        (Day20a.solve, "day20.txt","Day20a",0),
        (Day20b.solve, "day20.txt","Day20b",0),
        (Day21a.solve, "day21.txt","Day21a",0),
        (Day21b.solve, "day21.txt","Day21b",0),
        (Day22a.solve, "day22.txt","Day22a",0),
        (Day22b.solve, "day22.txt","Day22b",0),
        (Day23a.solve, "day23.txt","Day23a",0),
        (Day23b.solve, "day23.txt","Day23b",0),
        (Day24a.solve, "day24.txt","Day24a",0),
        (Day24b.solve, "day24.txt","Day24b",0),
        (Day25a.solve, "day25.txt","Day25a",0),
        (Day25b.solve, "day25.txt","Day25b",0)]

run :: (Text->Int, String,String,Int) -> IO ()
run (f,s,d,i) = do
            content <- Data.Text.IO.readFile $ pathJoin ["inputs",s]
            let res = f content
            print $ d ++ " " ++ (show res) ++ " " ++ (show $ res==i)

search :: String -> (Text->Int, String, String,Int)
search x = fromMaybe (L.head $ days) $ L.find (\(_,_,w,_) -> x==w) $ days

parseArgs :: [String] -> IO ()
parseArgs [] = mapM_ (run) $ days
parseArgs (x:_) = run $ search x

main :: IO ()
main = do
    args <- getArgs
    parseArgs args