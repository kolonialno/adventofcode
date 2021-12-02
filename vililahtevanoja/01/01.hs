windows :: Int -> [Int] -> [[Int]]
windows l xs = map (\i -> take l $ drop i xs) [0 .. length xs - l]

main :: IO ()
main = do
  inputs <- lines <$> readFile "input.txt"
  let ns = map read inputs :: [Int]
  let result1 = snd $ foldl (\(last, count) curr -> if curr > last then (curr, count + 1) else (curr, count)) (maxBound :: Int, 0) ns
  let result2 = snd $ foldl (\(lastSum, count) curr -> if sum curr > lastSum then (sum curr, count + 1) else (sum curr, count)) (maxBound :: Int, 0) (windows 3 ns)
  putStrLn $ "Part 1: " ++ show result1
  putStrLn $ "Part 2: " ++ show result2
