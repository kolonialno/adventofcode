module Main
  (
    main
  )
  where

import Prelude

import Data.Array (filter, index, length)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)

toTup :: String -> Maybe (Tuple Int Int)
toTup s = do
  let x = split (Pattern "-") s

  first <- index x 0 >>= fromString
  second <- index x 1 >>= fromString

  pure (Tuple first second)


contained :: Tuple Int Int -> Tuple Int Int -> Boolean
contained t1 t2 = contained'
  where
    xL = fst t1
    xR = snd t1
    yL = fst t2
    yR = snd t2
    contained' = (xL <= yL && xR >= yR) || (yL <= xL && yR >= xR)

overlaps :: Tuple Int Int -> Tuple Int Int -> Boolean
overlaps t1 t2 = contained'
  where
    xL = fst t1
    xR = snd t1
    yL = fst t2
    yR = snd t2
    contained' = (xL >= yL && xL <= yR) || (xR >= yL && xR <= yR) || (yL >= xL && yL <= xR) || (yR >= xL && yR <= xR)

calc :: (Tuple Int Int -> Tuple Int Int -> Boolean) -> Array (Maybe (Tuple Int Int)) -> Boolean
calc f x = fromMaybe false do
  first <- index x 0
  fTupl <- first
  secon <- index x 1
  sTupl <- secon

  pure $ f fTupl sTupl

main :: Effect Unit
main = do
  contents <- readTextFile Encoding.UTF8 "./input.txt"
  log $ show $ length $ filter (\x -> x) $ (map (calc contained <<< (map toTup)) <<< map (split (Pattern ",")) <<< split (Pattern "\n")) contents
  log $ show $ length $ filter (\x -> x) $ (map (calc overlaps <<< (map toTup)) <<< map (split (Pattern ",")) <<< split (Pattern "\n")) contents