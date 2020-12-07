module Utils where

import Data.List (intercalate)
import Data.List.Split (splitOn)

reformat :: String -> String
reformat = replace "bags" "bag" . replace "." "" . replace " no " " 0 "

parseRule :: String -> (String, [(Int, String)])
parseRule str = do
  let bag = head (splitOn " contain " str)
      capacity = parseCapacity (splitOn " contain " str !! 1)
  (bag, capacity)

parseCapacity :: String -> [(Int, String)]
parseCapacity str = do
  let bags = splitOn ", " str
  map parseBagCapacity bags

parseBagCapacity :: String -> (Int, String)
parseBagCapacity str = do
  let n = read $ take 1 str
      bag = drop 2 str
  (n, bag)

replace from to = intercalate to . splitOn from
