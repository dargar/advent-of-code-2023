import Control.Applicative ((<|>))
import Control.Arrow
import Data.Char (isLetter, isDigit)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Text.ParserCombinators.ReadP
import qualified Data.Map as M

main :: IO ()
main = interact $ parse >>> (uncurry part1 &&& uncurry part2) >>> present

parse :: String -> ([Char], Map String (String, String))
parse = fst . head . filter (null . snd) . readP_to_S parse'
  where
    parse' = do
      dirs <- many1 (char 'L' <|> char 'R') <* eol
      eol
      nodes <- node `sepBy` eol
      optional eol
      eof
      return (dirs, M.fromList nodes)

    node = do
      n <- word
      string " = "
      l <- char '(' *> word <* string ", "
      r <- word <* char ')'
      return (n, (l, r))

    word = many1 (satisfy isLetter <|> satisfy isDigit)
    eol = char '\n'

part1 :: [Char] -> Map String (String, String) -> Int
part1 dirs nodes = length $ takeWhile (/= "ZZZ") $ scanl step "AAA" $ cycle dirs
  where
    step n 'L' = fromJust $ fmap fst $ M.lookup n nodes
    step n 'R' = fromJust $ fmap snd $ M.lookup n nodes
    step _ _ = undefined

part2 :: [Char] -> Map String (String, String) -> Int
part2 dirs nodes = foldr1 lcm $ map (period dirs nodes) $ filter ((== 'A') . last) $ M.keys nodes
  where
    period dirs nodes n =
      let ps = map fst $ filter ((== 'Z') . last . snd) $ zip [0..] $ scanl step n $ cycle dirs in
      let qs = map (uncurry $ flip (-)) $ zip ps $ tail ps in
      fst $ head $ filter (uncurry (==)) $ zip qs $ tail qs

    step n 'L' = fromJust $ fmap fst $ M.lookup n nodes
    step n 'R' = fromJust $ fmap snd $ M.lookup n nodes
    step _ _ = undefined

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

