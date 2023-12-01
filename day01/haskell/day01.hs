import Control.Arrow
import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)

main :: IO ()
main = interact $ lines >>> solve numbersPart1 &&& solve numbersPart2 >>> present

solve :: (String -> [Int]) -> [String] -> Int
solve numbers = sum . map (uncurry (+) . first (* 10) . (head &&& last) . numbers)

numbersPart1 :: String -> [Int]
numbersPart1 = map digitToInt . filter isDigit

numbersPart2 :: String -> [Int]
numbersPart2 [] = []
numbersPart2 ns | "one"   `isPrefixOf` ns = 1 : (numbersPart2 $ tail ns)
                | "two"   `isPrefixOf` ns = 2 : (numbersPart2 $ tail ns)
                | "three" `isPrefixOf` ns = 3 : (numbersPart2 $ tail ns)
                | "four"  `isPrefixOf` ns = 4 : (numbersPart2 $ tail ns)
                | "five"  `isPrefixOf` ns = 5 : (numbersPart2 $ tail ns)
                | "six"   `isPrefixOf` ns = 6 : (numbersPart2 $ tail ns)
                | "seven" `isPrefixOf` ns = 7 : (numbersPart2 $ tail ns)
                | "eight" `isPrefixOf` ns = 8 : (numbersPart2 $ tail ns)
                | "nine"  `isPrefixOf` ns = 9 : (numbersPart2 $ tail ns)
                | isDigit (head ns) = (digitToInt $ head ns) : (numbersPart2 $ tail ns)
                | otherwise         = numbersPart2 $ tail ns

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

