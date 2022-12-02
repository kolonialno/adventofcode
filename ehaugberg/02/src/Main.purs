module Main where

import Prelude

import Data.Array (index)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)

data Shape = Rock | Paper | Scissors

toShape :: String -> Maybe Shape
toShape "A" = Just Rock
toShape "X" = Just Rock
toShape "B" = Just Paper
toShape "Y" = Just Paper
toShape "C" = Just Scissors
toShape "Z" = Just Scissors
toShape _   = Nothing

score :: Shape -> Shape -> Int
score Rock     Rock     = 1 + 3
score Rock     Paper    = 2 + 6
score Rock     Scissors = 3 + 0
score Paper    Rock     = 1 + 0
score Paper    Paper    = 2 + 3
score Paper    Scissors = 3 + 6
score Scissors Rock     = 1 + 6
score Scissors Paper    = 2 + 0
score Scissors Scissors = 3 + 3

shape :: Array String -> Maybe (Tuple Shape Shape)
shape arr = do
  firstStr  <- index arr 0
  first     <- toShape firstStr
  secondStr <- index arr 1
  second    <- toShape secondStr
  pure (Tuple first second)

calculate :: String -> Int
calculate = sum <<< map (fromMaybe 0 <<< scores <<< split (Pattern " ")) <<< split (Pattern "\n")
  where
    scores a = do
      sh <- shape a
      pure $ score (fst sh) (snd sh)

main :: Effect Unit
main = do
  contents <- readTextFile Encoding.UTF8 "./input.txt"

  log $ show $ calculate contents
