import Control.Applicative ((<$>), (<|>))
import Control.Arrow ((>>>), (&&&))
import Control.Monad (void)
import Data.Char (isDigit, isLetter)
import Data.Function (on)
import Data.List (sort, groupBy)
import Text.ParserCombinators.ReadP

data Game = Game { gameId :: Int
                 , gameTakes :: [[(String, Int)]]
                 }

main :: IO ()
main = interact $ parse >>> part1 &&& part2 >>> present

parse :: String -> [Game]
parse = fst . head . filter (null . snd) . readP_to_S games
  where
    games = game `sepBy` newline <* optional (newline *> eof)
    game = do
      gameId <- string "Game " *> number <* string ": "
      gameTakes <- takes `sepBy` string "; "
      return $ Game gameId gameTakes

    takes = colorCount `sepBy` string ", "
    colorCount = do
      n <- number
      char ' '
      color <- word
      return (color, n)

    number = read <$> munch1 isDigit
    word = munch1 isLetter
    newline = char '\n'

part1 :: [Game] -> Int
part1 = sum . map gameId . filter isPossibleGame
  where
    isPossibleGame = null . filter isImpossibleTake . concat . gameTakes
    isImpossibleTake ("red",   n) = n > 12
    isImpossibleTake ("green", n) = n > 13
    isImpossibleTake ("blue",  n) = n > 14
    isImpossibleTake _            = undefined

part2 :: [Game] -> Int
part2 = sum . map power . map fewestCubes
  where
    fewestCubes = map last . groupBy ((==) `on` fst) . sort . concat . gameTakes
    power = product . map snd

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

