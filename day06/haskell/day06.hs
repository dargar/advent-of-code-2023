import Control.Arrow
import Data.Char (isDigit)
import Data.List (singleton)

main :: IO ()
main = interact $ (parse values1 &&& parse values2) >>> both solve >>> present
  where
    both f (a, b) = (f a, f b)

parse :: (String -> [Int]) -> String -> [(Int, Int)]
parse values = uncurry zip . ((values . head) &&& (values . last)) . lines

values1, values2 :: String -> [Int]
values1 = map read . filter (all isDigit) . words
values2 = map read . singleton . filter isDigit

solve :: [(Int, Int)] -> Int
solve = product . map waysToWin
  where
    waysToWin (t, d) = length $ filter (> d) [s * (t - s) | s <- [0..t]]

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

