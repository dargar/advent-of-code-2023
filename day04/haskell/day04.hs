import Control.Arrow
import Data.Array (Array, accum, (!), elems, listArray, range)
import Data.Char (isDigit)
import Data.Set (Set, fromList, intersection, size)
import Text.ParserCombinators.ReadP

data Card = Card { cardId :: Int
                 , winning :: Set Int
                 , numbers :: Set Int
                 }
  deriving (Show)

main :: IO ()
main = interact $ parse >>> (part1 &&& part2) >>> present

parse :: String -> [Card]
parse = fst . head . filter (null . snd) . readP_to_S cards
  where
    cards = card `sepBy` newline <* optional newline <* eof
    card = do
      cid <- string "Card" *> whitespace *> number <* string ":"
      winning <- fromList <$> (whitespace *> number `sepBy` whitespace <* whitespace)
      string "|"
      numbers <- fromList <$> (whitespace *> number `sepBy` whitespace)
      return $ Card cid winning numbers

    number = read <$> munch1 isDigit
    whitespace = munch (== ' ')
    newline = char '\n'

part1 :: [Card] -> Int
part1 = sum . map ((2 ^) . pred) . filter (> 0) . map wins

part2 :: [Card] -> Int
part2 cards = sum
            $ elems
            $ foldl (\as (cid, ws) -> accum (+) as [(i, as ! cid) | i <- tail $ range (cid, cid + ws)]) arr
            $ filter ((> 0) . snd)
            $ map (cardId &&& wins) cards
  where
    bnds = (minimum &&& maximum) $ map cardId cards
    arr = listArray bnds $ repeat 1

wins :: Card -> Int
wins = size . uncurry intersection . (numbers &&& winning)

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

