import Control.Arrow

main :: IO ()
main = interact $ parse >>> (solve extrapolated1 &&& solve extrapolated2) >>> present

parse :: String -> [[Int]]
parse = map (map read) . map words . lines

solve :: ([Int] -> [Int]) -> [[Int]] -> Int
solve f = sum . map head . map f

extrapolated1 :: [Int] -> [Int]
extrapolated1 = extrapolated' . reverse
  where
    extrapolated' xs
      | all (== 0) xs = 0 : xs
      | otherwise =
        let xs' = extrapolated' $ map (uncurry (-)) $ zip xs $ tail xs in
        head xs + head xs' : xs

extrapolated2 :: [Int] -> [Int]
extrapolated2 = extrapolated'
  where
    extrapolated' xs
      | all (== 0) xs = 0 : xs
      | otherwise =
        let xs' = extrapolated' $ map (uncurry $ flip (-)) $ zip xs $ tail xs in
        head xs - head xs' : xs

present :: (Show a, Show b) => (a, b) -> String
present (a, b) = unlines [ "First answer: " ++ show a
                         , "Second answer: " ++ show b ]

