import Control.Arrow
import Data.Array.Unboxed
import Data.Function (on)
import Data.Ix
import Data.List (transpose, groupBy, sort)
import Data.Sequence (Seq((:<|)), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

main :: IO ()
main = interact $ parse >>> (part1 &&& part2) >>> present

parse :: String -> UArray (Int, Int) Char
parse input =
  let cs = lines input in
  let width = length $ head cs in
  let height = length cs in
  listArray ((0, 0), (width-1, height-1)) $ concat $ transpose cs

part1 :: UArray (Int, Int) Char -> Int
part1 grid = flip div 2 $ length $ path grid start
  where
    start = fst $ head $ filter ((== 'S') . snd) $ assocs grid

part2 :: UArray (Int, Int) Char -> Int
part2 grid = flip div 9
           $ countDots
           $ fillInsideLining start
           $ fillOutside
           $ onlyPath start
           $ g
           $ replaceStart
           $ grid
  where
    start = expandedStart grid

expandedStart grid = (\(x, y) -> (x * 3 + 1, y * 3 + 1)) $ fst $ head $ filter ((== 'S') . snd) $ assocs grid

path :: UArray (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
path grid start = traverse (head $ neighbors start $ grid ! start) start
  where
    gridBounds = bounds grid

    traverse prev curr =
      let next = head $ filter (/= prev) $ neighbors curr $ grid ! curr in
      if next == start then [curr] else curr : traverse curr next

    neighbors (x, y) '|' = filter (inRange gridBounds) [(x, y - 1), (x, y + 1)]
    neighbors (x, y) '-' = filter (inRange gridBounds) [(x - 1, y), (x + 1, y)]
    neighbors (x, y) 'L' = filter (inRange gridBounds) [(x + 1, y), (x, y - 1)]
    neighbors (x, y) 'J' = filter (inRange gridBounds) [(x - 1, y), (x, y - 1)]
    neighbors (x, y) '7' = filter (inRange gridBounds) [(x - 1, y), (x, y + 1)]
    neighbors (x, y) 'F' = filter (inRange gridBounds) [(x + 1, y), (x, y + 1)]
    neighbors (x, y) _   =
      let north = filter (flip elem "|7F" . (grid !)) $ filter (inRange gridBounds) [(x, y - 1)] in
      let south = filter (flip elem "|LJ" . (grid !)) $ filter (inRange gridBounds) [(x, y + 1)] in
      let west  = filter (flip elem "-LF" . (grid !)) $ filter (inRange gridBounds) [(x - 1, y)] in
      let east  = filter (flip elem "-J7" . (grid !)) $ filter (inRange gridBounds) [(x + 1, y)] in
      north ++ south ++ west ++ east

replaceStart :: UArray (Int, Int) Char -> UArray (Int, Int) Char
replaceStart grid = grid // [(start, replacement)]
  where
    start = fst $ head $ filter ((== 'S') . snd) $ assocs grid
    gridBounds = bounds grid
    replacement = neighbors start

    neighbors (x, y) =
      let north = filter (flip elem "|7F" . (grid !)) $ filter (inRange gridBounds) [(x, y - 1)] in
      let south = filter (flip elem "|LJ" . (grid !)) $ filter (inRange gridBounds) [(x, y + 1)] in
      let west  = filter (flip elem "-LF" . (grid !)) $ filter (inRange gridBounds) [(x - 1, y)] in
      let east  = filter (flip elem "-J7" . (grid !)) $ filter (inRange gridBounds) [(x + 1, y)] in
      case [north, south, west, east] of
        [[_], [_], [], []] -> '|'
        [[_], [], [_], []] -> 'J'
        [[_], [], [], [_]] -> 'L'
        [[], [_], [], [_]] -> 'F'
        [[], [_], [_], []] -> '7'
        [[], [], [_], [_]] -> '-'
        _ -> undefined

g :: UArray (Int, Int) Char -> UArray (Int, Int) Char
g arr = array ((0,0), bs) $ sort $ concat $ map (uncurry f) $ assocs arr
  where
    bs = (((+ 2) . (* 3)) *** ((+ 2) . (* 3))) $ snd $ bounds arr

f :: (Int, Int) -> Char -> [((Int, Int), Char)]
f (x, y) c =
  let x' = x * 3 in
  let y' = y * 3 in
  let ps = map (\(a, b) -> (b, a)) $ range ((y', x'), (y' + 2, x' + 2)) in
  case c of
    '|' -> zip ps $ ['.', '|', '.',
                     '.', '|', '.',
                     '.', '|', '.']

    '-' -> zip ps $ ['.', '.', '.',
                     '-', '-', '-',
                     '.', '.', '.']

    'L' -> zip ps $ ['.', '|', '.',
                     '.', 'L', '-',
                     '.', '.', '.']

    'J' -> zip ps $ ['.', '|', '.',
                     '-', 'J', '.',
                     '.', '.', '.']

    '7' -> zip ps $ ['.', '.', '.',
                     '-', '7', '.',
                     '.', '|', '.']

    'F' -> zip ps $ ['.', '.', '.',
                     '.', 'F', '-',
                     '.', '|', '.']

    '.' -> zip ps $ ['.', '.', '.',
                     '.', '.', '.',
                     '.', '.', '.']
    unknown -> undefined

showGrid :: UArray (Int, Int) Char -> String
showGrid grid = unlines $ map (map snd) $ transpose $ groupBy ((==) `on` (fst . fst)) $ assocs $ grid

fillOutside grid = grid // os
  where
    os = flip zip (repeat 'O') $ floodFill (0, 0) grid

floodFill :: (Int, Int) -> UArray (Int, Int) Char -> [(Int, Int)]
floodFill start grid = go (Seq.singleton start) (Set.empty) []
  where
    neighbors (x, y) = filter (inRange $ bounds grid) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

    go Seq.Empty _ result = result
    go (next :<| frontier) visisted result =
      let ns = filter ((== '.') . (grid !)) $ filter (not . flip Set.member visisted) $ neighbors next in
      go (frontier >< Seq.fromList ns) (Set.union visisted $ Set.fromList $ next:ns) (next:result)

fillInsideLining s grid = grid // is
  where
    pat = Set.fromList $ path grid s
    start = fst $ head $ filter ((== '.') . snd) $ assocs grid
    is = flip zip (repeat 'I') $ filter nextToPath $ floodFill start grid

    nextToPath p = any (flip Set.member pat) $ neighbors p

    neighbors (x, y) = filter (inRange $ bounds grid) $ filter (/= (x, y)) $ range ((x-1, y-1), (x+1, y+1))

countDots grid = length $ filter (== '.') $ elems grid

onlyPath s grid = grid // xs
  where
    pat = Set.fromList $ path grid s
    xs = map (second (\_ -> '.')) $ filter (not . flip Set.member pat . fst) $ assocs grid

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

