import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Control.Applicative ((<$>), (<|>))
import Control.Arrow
import Data.List (sort, groupBy)

data Cube = Red Int
          | Green Int
          | Blue Int
  deriving (Show, Eq, Ord)

main :: IO ()
main = interact $ parse >>> part1 &&& part2 >>> present

parse :: String -> [(Int, [[Cube]])]
parse = fst . head . filter (null . snd) . readP_to_S games
  where
    games = game `sepBy` eol <* optional (char '\n')
    game = do
      game <- string "Game " *> number <* string ": "
      cs <- cubes `sepBy` string "; "
      return (game, cs)

    cubes = cube `sepBy` string ", "
    cube = red <|> green <|> blue

    red = Red <$> number <* string " red"
    green = Green <$> number <* string " green"
    blue = Blue <$> number <* string " blue"

    number = read <$> munch1 isDigit
    eol = choice [ eof, char '\n' >> return () ]

part1 :: [(Int, [[Cube]])] -> Int
part1 = sum . map fst . filter possible
  where
    possible = null . filter isImpossible . concat . snd
    isImpossible (Red n)   = n > 12
    isImpossible (Green n) = n > 13
    isImpossible (Blue n)  = n > 14

part2 :: [(Int, [[Cube]])] -> Int
part2 = sum . map power . map smallestPossible
  where
    smallestPossible = map last . groupBy matchingColor . sort . concat . snd
    
    matchingColor (Red _)   (Red _)   = True
    matchingColor (Green _) (Green _) = True
    matchingColor (Blue _)  (Blue _)  = True
    matchingColor _         _         = False
    
    power = product . map colorValue
    
    colorValue (Red n)   = n
    colorValue (Green n) = n
    colorValue (Blue n)  = n

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

