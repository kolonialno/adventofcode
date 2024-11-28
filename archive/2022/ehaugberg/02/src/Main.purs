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

data Shape  = Rock | Paper | Scissors
data Result = Win | Draw | Loss

toResult :: String -> Maybe Result
toResult "X" = Just Loss
toResult "Y" = Just Draw
toResult "Z" = Just Win
toResult  _  = Nothing

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

-- part two score calc
scoreTwo :: Shape -> Result -> Int
scoreTwo Rock     Win   = 2 + 6
scoreTwo Rock     Draw  = 1 + 3
scoreTwo Rock     Loss  = 3 + 0
scoreTwo Paper    Win   = 3 + 6
scoreTwo Paper    Draw  = 2 + 3
scoreTwo Paper    Loss  = 1 + 0
scoreTwo Scissors Win   = 1 + 6
scoreTwo Scissors Draw  = 3 + 3
scoreTwo Scissors Loss  = 2 + 0

-- part one shaping
shape :: Array String -> Maybe (Tuple Shape Shape)
shape arr = do
  firstStr  <- index arr 0
  first     <- toShape firstStr
  secondStr <- index arr 1
  second    <- toShape secondStr
  pure (Tuple first second)

-- part two shaping
shapeTwo :: Array String -> Maybe (Tuple Shape Result)
shapeTwo arr = do
  firstStr  <- index arr 0
  first     <- toShape firstStr
  secondStr <- index arr 1
  second    <- toResult secondStr
  pure (Tuple first second)

partOne :: String -> Int
partOne = sum <<< map (fromMaybe 0 <<< scores <<< split (Pattern " ")) <<< split (Pattern "\n")
  where
    scores a = do
      sh <- shape a
      pure $ score (fst sh) (snd sh)

partTwo :: String -> Int
partTwo = sum <<< map (fromMaybe 0 <<< scores <<< split (Pattern " ")) <<< split (Pattern "\n")
  where
    scores a = do
      sh <- shapeTwo a
      pure $ scoreTwo (fst sh) (snd sh)

main :: Effect Unit
main = do
  contents <- readTextFile Encoding.UTF8 "./input.txt"

  log $ show $ partOne contents
  log $ show $ partTwo contents
