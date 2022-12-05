module Main
  ( Move(..)
  , calc
  , getInitStacks
  , getMoves
  , getStacks
  , main
  , reverse
  )
  where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (toArray, updateAt)
import Data.Either (hush)
import Data.Int (decimal, fromString, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), fromCodePointArray, replace, split, splitAt, toCodePointArray, trim, uncons)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (global, noFlags)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)

reverse :: String -> String
reverse = (fromCodePointArray <<< Array.reverse <<< toCodePointArray)

data Move = Move Int Int Int

instance showMove :: Show Move where
  show (Move a b c) = toStringAs decimal a <> " " <> toStringAs decimal b <> " " <> toStringAs decimal c

-- only works with this specific input
getStacks :: String -> Array String
getStacks = Array.reverse <<< splitEvery []
  where
    splitEvery :: Array String -> String -> Array String
    splitEvery acc s = case splitAt 4 s of
      { before: b, after: ""} -> Array.cons (clean b) acc
      { before: b, after: af} -> splitEvery (Array.cons (clean b) acc) af

    clean = replace (Pattern "]") (Replacement "") <<< replace (Pattern "[") (Replacement "") <<< trim

getInitStacks :: Array String -> Array String
getInitStacks xs = combine $ map getStacks (Array.splitAt 8 xs).before
  where
    combine :: Array (Array String) -> Array String
    combine stacks = case Array.uncons stacks of
      Just { head: h, tail: t } -> Array.foldl (Array.zipWith (<>)) h t
      Nothing -> []

getMoves ∷ Array String -> Array (Move)
getMoves xs = map (fromMaybe (Move 0 0 0) <<< toMove) (Array.splitAt 3 (Array.splitAt 7 xs).after).after
  where
    toMove :: String -> Maybe Move
    toMove s = do
      r <- hush $ regex "move ([0-9]+) from ([0-9]+) to ([0-9]+)" noFlags
      x <- match r s

      let moves = toArray $ map (fromMaybe 0 <<< fromString <<< fromMaybe "") x

      first <- Array.index moves 1
      secon <- Array.index moves 2
      third <- Array.index moves 3

      pure $ Move first secon third

makeMoveOne :: Move -> Array String -> Array String
makeMoveOne (Move a b c) xs = fromMaybe [] do
  fromStack <- Array.index xs (b - 1)
  toStack   <- Array.index xs (c - 1)

  let spl = splitAt a fromStack
  let upd = (reverse spl.before) <> toStack

  updateOne <- Array.updateAt (b - 1) spl.after xs
  updateTwo <- Array.updateAt (c - 1) upd updateOne

  pure updateTwo

makeMoveTwo :: Move -> Array String -> Array String
makeMoveTwo (Move a b c) xs = fromMaybe [] do
  fromStack <- Array.index xs (b - 1)
  toStack   <- Array.index xs (c - 1)

  let spl = splitAt a fromStack
  let upd = spl.before <> toStack

  updateOne <- Array.updateAt (b - 1) spl.after xs
  updateTwo <- Array.updateAt (c - 1) upd updateOne

  pure updateTwo

calc :: (Move -> Array String -> Array String) -> Array Move -> Array String -> Array String
calc makeMove mvs str = Array.foldl (\a b -> makeMove b a) str mvs

main :: Effect Unit
main = do
  contents <- readTextFile Encoding.UTF8 "./input.txt"

  let splitByNewLine = split (Pattern "\n") contents
  let stacks = getInitStacks splitByNewLine
  let moves = getMoves splitByNewLine

  log $ show $ calc makeMoveOne moves stacks
  log $ show $ calc makeMoveTwo moves stacks