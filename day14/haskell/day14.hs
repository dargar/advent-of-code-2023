import Control.Arrow
import Data.Function (on)
import Data.List (transpose, groupBy, sort)
import Data.Map ((!?))
import qualified Data.Map as M

main :: IO ()
main = interact $ parse >>> (part1 &&& part2) >>> present

parse :: String -> [[Char]]
parse = lines

part1 :: [[Char]] -> Int
part1 = totalLoad . tilt

part2 :: [[Char]] -> Int
part2 = totalLoad . extrapolate 1000000000 . findRepetition M.empty . tail . zip [0..] . iterate spinCycle
  where
    extrapolate n (start, end, p) =
      head $ drop ((n - start) `mod` (end - start)) $ iterate spinCycle p

    findRepetition m ((n, p):ps) =
      case m !? p of
        Just n' -> (n', n, p)
        Nothing -> findRepetition (M.insert p n m) ps

tilt :: [[Char]] -> [[Char]]
tilt = transpose . map (concatMap (reverse . sort)) . map (groupBy ((==) `on` (== '#'))) . transpose

totalLoad :: [[Char]] -> Int
totalLoad = sum . map (uncurry (*)) . zip [1..] . map (length . filter (== 'O')) . reverse

spinCycle :: [[Char]] -> [[Char]]
spinCycle = head . drop 4 . iterate (rotate . tilt)

rotate :: [[Char]] -> [[Char]]
rotate = map reverse . transpose

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

