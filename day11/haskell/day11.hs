import Control.Arrow
import Data.List (transpose, tails)

main :: IO ()
main = interact $ (parse 2 &&& parse 1000000) >>> both solve >>> present
  where
    both f (a, b) = (f a, f b)

parse :: Int -> String -> [(Int, Int)]
parse m input = map fst
              $ filter ((== '#') . snd)
              $ concatMap (\(y, line) -> zipWith (\x c -> ((x, y), c)) xs line)
              $ zip ys
              $ lines
              $ input
  where
    xs = scanl f 0 $ transpose $ lines input
    ys = scanl f 0 $ lines input
    f n line = if all (== '.') line then n + m else n + 1

solve :: [(Int, Int)] -> Int
solve xs = sum [manhattan a b | (a:bs) <- tails xs, b <- bs]
  where
    manhattan (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

