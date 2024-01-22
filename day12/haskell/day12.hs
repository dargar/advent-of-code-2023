import Control.Arrow
import Data.List (intercalate)
import Data.MemoTrie

main :: IO ()
main = interact $ parse >>> (part1 &&& part2) >>> present

parse :: String -> [([Char], [Int])]
parse = map (head &&& numbers . last) . map words . lines
  where
    numbers :: String -> [Int]
    numbers [] = []
    numbers xs =
      let (digits, rest) = span (/= ',') xs in
      read digits : (numbers $ drop 1 rest)

part1 :: [([Char], [Int])] -> Int
part1 = sum . map (uncurry arrangements)

part2 :: [([Char], [Int])] -> Int
part2 = sum . map (uncurry arrangements) . map expand

arrangements :: [Char] -> [Int] -> Int
arrangements = memo2 aux
  where
    aux cs []
      | any (== '#') cs = 0
      | otherwise       = 1
    aux [] rs
      | null rs   = 1
      | otherwise = 0
    aux ('?':cs) rs = arrangements ('.':cs) rs + arrangements ('#':cs) rs
    aux ('.':cs) rs = arrangements cs rs
    aux cs (r:rs)
      | (xs, ys) <- splitAt r cs
      , length xs == r
      , all (/= '.') xs
      , null ys || head ys /= '#'
      = arrangements (drop 1 ys) rs
      | otherwise = 0

expand :: ([Char], [Int]) -> ([Char], [Int])
expand = (intercalate ['?'] . replicate 5 *** concat . replicate 5)

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

