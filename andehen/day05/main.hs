import Data.Text as Text (lines, unpack)
import Data.Text.IO as TextIO (readFile)

main = do
  input <- fmap Text.lines (TextIO.readFile "boarding_passes.txt")
  let boarding_passes = map Text.unpack input
      seat_ids = map getSeatId boarding_passes

  print $ maximum seat_ids

  let all_seats = [minimum seat_ids .. maximum seat_ids]
      my_seat = head (filter isFree all_seats)
        where isFree s = notElem s seat_ids

  print $ my_seat

getSeatId :: [Char] -> Int
getSeatId boarding_pass = do
  let row = binarySearch 0 127 (convertCharsToBinary 'F' 'B' (take 7 boarding_pass))
      column = binarySearch 0 7 (convertCharsToBinary 'L' 'R' (drop 7 boarding_pass))
  row * 8 + column

binarySearch :: Int -> Int -> [Int] -> Int
binarySearch lower upper binary = do
  let c = headOrNeg binary
      xs = tail binary
      half = (upper - lower) `div` 2
  case c of
    0 -> binarySearch lower (lower + half) xs
    1 -> binarySearch (lower + half) upper xs
    -1 -> upper

headOrNeg :: [Int] -> Int
headOrNeg [] = -1
headOrNeg xs = head xs

convertCharsToBinary :: Char -> Char -> [Char] -> [Int]
convertCharsToBinary l h str = map (convertCharToBinary l h) str

convertCharToBinary :: Char -> Char -> Char -> Int
convertCharToBinary l h c =
  if c == l
    then 0
    else
      if c == h
        then 1
        else -1
