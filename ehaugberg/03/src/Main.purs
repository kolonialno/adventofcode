module Main
  ( inAllCompartments
  , inBothCompartments
  , main
  , score
  , splitByThree
  )
  where

import Prelude

import Data.Array (concat)
import Data.Array (concat, cons, filter, head, index, nub, splitAt) as Array
import Data.Enum (fromEnum)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (CodePoint, Pattern(..), contains, length, singleton, split, splitAt, toCodePointArray, uncons)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)

score :: Int -> Int
score x = if x >= 97 && x <= 122 then x - 96 else x - 38

inBothCompartments :: Tuple String String -> Array Int
inBothCompartments s = map (score <<< fromEnum) $ Array.nub (inBothCompartments' [] (fst s) (snd s))
  where
    inBothCompartments' :: Array CodePoint -> String -> String -> Array CodePoint
    inBothCompartments' arr st st_ = case uncons st of
      Just { head: x, tail: xs } -> inBothCompartments' (if contains (Pattern (singleton x)) st_ then [x] <> arr else arr) xs st_
      Nothing -> arr

inAllCompartments :: Array String -> Int
inAllCompartments s = sum $ fromMaybe [] f
  where
    f :: Maybe (Array Int)
    f = do
      first  <- Array.index s 0
      second <- Array.index s 1
      third  <- Array.index s 2

      pure $ map calcScore $ Array.concat $ map toCodePointArray $ Array.nub $ Array.filter (\s -> contains (Pattern s) second && contains (Pattern s) third) (split (Pattern "") first)
    calcScore :: CodePoint -> Int
    calcScore = score <<< fromEnum

strSplit :: String -> Tuple String String
strSplit s = Tuple x.before x.after
  where
    x = splitAt splitPos s
    splitPos = (length s) / 2

splitByThree :: Array String -> Array (Array String)
splitByThree arr = splitByThree' arr ([] :: Array (Array String))
  where
    splitByThree' :: Array String -> Array (Array String) -> Array (Array String)
    splitByThree' xs acc = case Array.splitAt 3 xs of
      { before: b, after: []} -> Array.cons b acc
      { before: b, after: af} -> splitByThree' af (Array.cons b acc)

main :: Effect Unit
main = do
  contents <- readTextFile Encoding.UTF8 "./input.txt"

  log $ show $ sum $ Array.concat $ map (inBothCompartments <<< strSplit) $ split (Pattern "\n") contents
  log $ show $ sum $ map inAllCompartments $ splitByThree (split (Pattern "\n") contents)
