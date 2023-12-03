import Control.Arrow
import Data.Char (isDigit)

main :: IO ()
main = interact $ parse >>> (part1 &&& part2) >>> present

parse :: String -> ([(Int, ((Int, Int), (Int, Int)))], [(Char, (Int, Int))])
parse = (concat *** concat) . unzip . map (uncurry parseLine) . zip [0..] . lines

parseLine :: Int -> String -> ([(Int, ((Int, Int), (Int, Int)))], [(Char, (Int, Int))])
parseLine y = go [] [] . zip [0..]
  where
    go ns ss []             = (ns, ss)
    go ns ss ((_, '.'): cs) = go ns ss cs
    go ns ss ((n, c): cs) | isDigit c = let (ds, rs) = span (isDigit . snd) cs in
                                        go ((read (c:map snd ds), ((n, y), (n + length ds, y))):ns) ss rs
                          | otherwise = go ns ((c,(n,y)):ss) cs

part1 :: ([(Int, ((Int, Int), (Int, Int)))], [(Char, (Int, Int))]) -> Int
part1 (ns, ss) = sum
               $ map fst
               $ filter (\(_, n) -> any (within n) $ map snd ss)
               $ map (second adjacent) ns

part2 :: ([(Int, ((Int, Int), (Int, Int)))], [(Char, (Int, Int))]) -> Int
part2 (ns, ss) = sum
               $ map product
               $ filter ((== 2) . length)
               $ map (\(_, p) -> map fst . filter (flip within p . snd) $ map (second adjacent) ns)
               $ filter ((== '*') . fst) ss

adjacent :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
adjacent ((x0, y0), (x1, y1)) = ((x0 - 1, y0 - 1), (x1 + 1, y1 + 1))

within :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
within ((ax0, ay0), (ax1, ay1)) (px, py) = (ax0 <= px && px <= ax1) && (ay0 <= py && py <= ay1)

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

