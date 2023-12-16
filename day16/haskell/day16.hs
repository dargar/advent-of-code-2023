import Control.Arrow
import Data.Array
import Data.List (transpose)
import qualified Data.Set as S

main :: IO  ()
main = interact $ parse >>> (part1 &&& part2) >>> present

parse :: String -> Array (Int, Int) Char
parse string = listArray ((0, 0), (width-1, height-1)) $ concat $ transpose $ lines string
  where
    width = length $ head $ lines string
    height = length $ lines string

part1 :: Array (Int, Int) Char -> Int
part1 grid = energized grid (0, 0) (1, 0)

part2 :: Array (Int, Int) Char -> Int
part2 grid = maximum
           $ map (\(p, d) -> energized grid p d)
           $ [((0, y), d) | y <- [0..height], d <- [(1, 0), (-1, 0)]] ++
             [((x, 0), d) | x <- [0..width],  d <- [(0, 1), (0, -1)]]
  where
    (width, height) = snd $ bounds grid

energized :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Int
energized grid pos dir = S.size $ S.fromList $ path [(pos, dir)] S.empty
  where
    path [] _ = []
    path ((p, d):frontier) visited
      | S.member (p, d) visited =
        path frontier visited
      | otherwise =
        let
          pds = filter (inRange (bounds grid) . fst)
              $ filter (not . flip S.member visited)
              $ map (step p &&& id)
              $ dirs (grid ! p) d
        in
        p : path (pds ++ frontier) (S.insert (p, d) visited)

    dirs '|'  (_, 0) = [(0, -1), (0, 1)]
    dirs '-'  (0, _) = [(-1, 0), (1, 0)]
    dirs '/'  (x, y) = [(-y, -x)]
    dirs '\\' (x, y) = [(y,  x)]
    dirs _    d      = [d]

    step (x, y) (dx, dy) = (x + dx, y + dy)

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

