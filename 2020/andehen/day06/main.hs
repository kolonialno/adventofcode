{-# LANGUAGE OverloadedStrings #-}

import Data.List (intersect, nub)
import Data.Text as Text (Text, splitOn, unpack)
import Data.Text.IO as TextIO (readFile)

main = do
  input <- TextIO.readFile "answers.txt"
  let group_answers = parseInput input
  print $ sum (map (length . nub) (map (foldr (++) "") group_answers))
  print $ sum (map countAllYes group_answers)

parseInput :: Text.Text -> [[String]]
parseInput input =
  map (map Text.unpack) (map (splitOn "\n") (splitOn "\n\n" input))

countAllYes :: [String] -> Int
countAllYes xs = length $ foldr intersect (foldr (++) "" xs) xs
