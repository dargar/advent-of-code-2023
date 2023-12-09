import Control.Arrow
import Data.Function (on)
import Data.List (sort, sortBy, group, elemIndex, minimumBy)
import Data.Maybe (fromJust)

main :: IO ()
main = interact $ parse >>> (solve part1 &&& solve part2) >>> present

parse :: String -> [([Char], Int)]
parse = map ((head &&& (read . last)) . words) . lines

solve :: ([Char] -> [Char] -> Ordering) -> [([Char], Int)] -> Int
solve f = sum
        . map (uncurry (*))
        . zip [1..]
        . map snd
        . reverse
        . sortBy (f `on` fst)

part1 :: [Char] -> [Char] -> Ordering
part1 = (compare `on` handType) <> (compare `on` handValues)
  where
    handValues :: [Char] -> [Int]
    handValues = map (fromJust . flip elemIndex "AKQJT98765432")

part2 :: [Char] -> [Char] -> Ordering
part2 = (compare `on` (handType . jokerHand)) <> (compare `on` handValues)
  where
    handValues :: [Char] -> [Int]
    handValues = map (fromJust . flip elemIndex "AKQT98765432J")

    jokerHand :: [Char] -> [Char]
    jokerHand = minimumBy (compare `on` handType) . expand
      where
        strengths = "AKQT98765432"

        expand :: [Char] -> [[Char]]
        expand [] = [[]]
        expand ('J':cs) = foldr (\e acc -> zipWith (:) strengths (repeat e) ++ acc) [] $ expand cs
        expand (c:cs) = map (c:) $ expand cs

handType :: [Char] -> Int
handType = toType . group . sort
  where
    toType cs | fiveOfAKind cs  = 1
              | fourOfAKind cs  = 2
              | fullHouse cs    = 3
              | threeOfAKind cs = 4
              | twoPair cs      = 5
              | onePair cs      = 6
              | otherwise       = 7
    fiveOfAKind  = any ((== 5) . length)
    fourOfAKind  = any ((== 4) . length)
    fullHouse cs = any ((== 3) . length) cs && any ((== 2) . length) cs
    threeOfAKind = any ((== 3) . length)
    twoPair      = (== 2) . length . filter (== 2) . map length
    onePair      = (== 1) . length . filter (== 2) . map length

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

