import Control.Arrow
import Data.List (transpose)
import Data.List.Split (splitOn)

main :: IO ()
main = interact $ parse >>> (solve 0 &&& solve 1) >>> present

parse :: String -> [String]
parse = splitOn "\n\n"

solve :: Int -> [String] -> Int
solve n =
  sum . map (uncurry (+) . (solve' id transpose &&& solve' (* 100) id) . lines)
  where
    solve' f g = f
               . (\(xs, ys) -> if null ys then 0 else 1 + length xs)
               . span (/= n)
               . mirrorDiffs
               . g

mirrorDiffs :: [String] -> [Int]
mirrorDiffs (x:xs) = mirrorDiffs' [x] xs
  where
    mirrorDiffs' xs [] = []
    mirrorDiffs' xs ys =
      (sum $ zipWith countDiffs xs ys) : mirrorDiffs' (head ys : xs) (tail ys) 

    countDiffs :: Eq a => [a] -> [a] -> Int
    countDiffs = (length . filter (not . id)) .: zipWith (==)

    (.:) = (.) . (.)

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

