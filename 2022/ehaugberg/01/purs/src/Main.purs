module Main where

import Prelude

import Data.Array (groupBy, head, reverse, sort, take)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)

sums :: String -> Array Int
sums = reverse <<< sort <<< map (sum <<< parse) <<< groupNonEmpty <<< split (Pattern "\n") 
  where
    groupNonEmpty = groupBy (\a b -> a /= "" && b /= "")
    parse = map (fromMaybe 0 <<< fromString)

main :: Effect Unit
main = do
  contents <- readTextFile Encoding.UTF8 "../input.txt"
  log $ show $ fromMaybe 0 $ head $ sums contents
  log $ show $ sum $ take 3 $ sums contents

