import Data.Char (isAlpha, isDigit)
import Text.ParserCombinators.ReadP
  ( ReadP,
    munch,
    readP_to_S,
    string,
  )

data Direction = Forward | Up | Down | Unknown deriving (Eq, Show)

toDirection :: String -> Direction
toDirection "forward" = Forward
toDirection "up" = Up
toDirection "down" = Down
toDirection _ = Unknown

data Movement = Movement
  { direction :: Direction,
    amount :: Int
  }
  deriving (Eq, Show)

data Position = Position
  { horizontal :: Int,
    depth :: Int,
    aim :: Int
  }
  deriving (Eq, Show)

movementP :: ReadP Movement
movementP = do
  direction <- munch isAlpha
  _ <- string " "
  amount <- munch isDigit
  return (Movement (toDirection direction) (read amount :: Int))

updatePositionPart1 :: Position -> Movement -> Position
updatePositionPart1 pos (Movement Forward x) = Position (horizontal pos + x) (depth pos) 0
updatePositionPart1 pos (Movement Up x) = Position (horizontal pos) (depth pos - x) 0
updatePositionPart1 pos (Movement Down x) = Position (horizontal pos) (depth pos + x) 0
updatePositionPart1 pos (Movement Unknown _) = error "Unkown movement"

updatePositionPart2 :: Position -> Movement -> Position
updatePositionPart2 pos (Movement Forward x) = Position (horizontal pos + x) (depth pos + aim pos * x) (aim pos)
updatePositionPart2 pos (Movement Up x) = Position (horizontal pos) (depth pos) (aim pos - x)
updatePositionPart2 pos (Movement Down x) = Position (horizontal pos) (depth pos) (aim pos + x)
updatePositionPart2 pos (Movement Unknown _) = error "Unkown movement"

main :: IO ()
main = do
  inputs <- lines <$> readFile "input.txt"
  let movements = map fst $ concatMap (readP_to_S movementP) inputs
  let part1Position = foldl updatePositionPart1 (Position 0 0 0) movements
  let result1 = horizontal part1Position * depth part1Position
  putStrLn $ "Part 1: " ++ show result1
  let part2Position = foldl updatePositionPart2 (Position 0 0 0) movements
  let result2 = horizontal part2Position * depth part2Position
  putStrLn $ "Part 2: " ++ show result2
