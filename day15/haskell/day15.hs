import Control.Arrow
import Data.Array
import Data.Char (ord, isLetter)
import Data.List.Split (splitOn)

main :: IO ()
main = interact $ parse >>> (part1 &&& part2) >>> present

parse :: String -> [String]
parse = splitOn "," . filter (/= '\n')

part1 :: [String] -> Int
part1 = sum . map hash

part2 :: [String] -> Int
part2 = sum
      . concat
      . map (uncurry $ zipWith (*))
      . map (repeat . (+ 1) *** (map (uncurry (*)) . zip [1..] . map snd))
      . filter (not . null . snd)
      . assocs
      . foldl update (listArray (0,256) $ repeat [])
  where
    update arr op =
      case span isLetter op of
        (label, "-") ->
          accum remove arr [(hash label, label)]
        (label, ('=':ns)) ->
          accum replace arr [(hash label, (label, read ns))]

    remove ys label = filter ((/= label) . fst) ys

    replace ys (label, n) =
      let (hs, ts) = span ((/= label) . fst) ys in
      hs ++ ((label, n) : drop 1 ts)

hash :: String -> Int
hash = foldl (flip rem 256 . (* 17) . uncurry (+) .: curry (second ord)) 0

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

(.:) = (.) . (.)
infixr 8 .:

