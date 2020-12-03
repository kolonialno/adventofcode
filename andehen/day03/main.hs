{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

main :: IO ()
main = do
  tree_map <- fmap Text.lines (TextIO.readFile "map.txt")
  let map_strings = map Text.unpack tree_map

  print $ "Part one: " ++ show (countTreesWithSlope map_strings [1, 3])

  print $
    "Part two: "
      ++ show
        ( foldr
            (*)
            1
            ( map
                (countTreesWithSlope map_strings)
                [[1, 1], [1, 3], [1, 5], [1, 7], [2, 1]]
            )
        )

countTreesWithSlope :: [String] -> [Int] -> Int
countTreesWithSlope map_strings slope =
  countTrees
    map_strings
    (length $ map_strings !! 0)
    (slope !! 0)
    (slope !! 1)
    0
    0

countTrees :: [String] -> Int -> Int -> Int -> Int -> Int -> Int
countTrees map_strings n x y i j =
  if i >= length map_strings
    then 0
    else
      (countTree (map_strings !! i) n j)
        + (countTrees map_strings n x y (i + x) (j + y))

countTree :: String -> Int -> Int -> Int
countTree str n j = fromEnum ((str !! (j `mod` n)) == '#')
