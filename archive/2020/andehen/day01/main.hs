import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

main :: IO ()
main = do
  numbers <- fmap Text.lines (TextIO.readFile "numbers.txt")

  let result1a = solution 2 numbers
  print $ "1a: " ++ show result1a

  let result1b = solution 3 numbers
  print $ "1b: " ++ show result1b

-- Returns the product of the subsequences of size k that sums to 2020
solution k numbers = foldr (*) 1 (concat (take 1 (filter sum2020 (subsequencesOfSize k (map textToInt numbers)))))
  where
    sum2020 xs = foldr (+) 0 xs == 2020

-- A function to find all subsequences of size n in a list xs
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
  let l = length xs
   in if (n > l)
        then []
        else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs) =
      let next = subsequencesBySize xs
       in zipWith
            (++)
            ([] : next)
            (map (map (x :)) next ++ [[]])

textToInt :: Text.Text -> Int
textToInt a = read $ Text.unpack a :: Int
