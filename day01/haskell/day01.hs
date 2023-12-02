import Control.Arrow
import Control.Monad (mfilter)
import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe, listToMaybe)

main :: IO ()
main = interact $ lines >>> solve numbersPart1 &&& solve numbersPart2 >>> present

solve :: (String -> [Int]) -> [String] -> Int
solve numbers = sum . map (uncurry (+) . first (* 10) . (head &&& last) . numbers)

numbersPart1 :: String -> [Int]
numbersPart1 = map digitToInt . filter isDigit

numbersPart2 :: String -> [Int]
numbersPart2 = mapMaybe number . tails
  where
    number ns | "one"   `isPrefixOf` ns = Just 1
              | "two"   `isPrefixOf` ns = Just 2
              | "three" `isPrefixOf` ns = Just 3
              | "four"  `isPrefixOf` ns = Just 4
              | "five"  `isPrefixOf` ns = Just 5
              | "six"   `isPrefixOf` ns = Just 6
              | "seven" `isPrefixOf` ns = Just 7
              | "eight" `isPrefixOf` ns = Just 8
              | "nine"  `isPrefixOf` ns = Just 9
              | otherwise = fmap digitToInt $ mfilter isDigit $ listToMaybe ns

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

