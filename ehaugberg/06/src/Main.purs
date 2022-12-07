module Main
  ( main
  )
  where

import Prelude

import Data.Array as Array
import Data.List.ZipList (ZipList)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (CodePoint, Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)

uniqueWindow :: forall a. Ord a => Int -> Array (a) -> Maybe Int
uniqueWindow = uniqueWindow' 0
  where
    uniqueWindow' :: Int -> Int -> Array (a) -> Maybe Int
    uniqueWindow' ind x arr = do
      let window = Array.take x arr

      if Array.length (Array.nub window) ==  x then pure $ ind + x else uniqueWindow' (ind + 1) x (fromMaybe [] $ Array.tail arr)

main :: Effect Unit
main = do
  contents <- readTextFile Encoding.UTF8 "./input.txt"

  log $ show $ fromMaybe 0 $ uniqueWindow 4 $ split (Pattern "") contents
  log $ show $ fromMaybe 0 $ uniqueWindow 14 $ split (Pattern "") contents
