
{-|
Count occurrences of element in list

>>> countOccurrences [1,2,3,3,4] 3
2
-}
countOccurrences :: (Eq a) => [a] -> a -> Int
countOccurrences s c = foldl (\count el -> if el == c then count + 1 else count) 0 s

{-
Check if list is prefix of list

>>> isPrefixOf [1,2] [1,2,3,4]
True
-}
isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _              = True
isPrefixOf _ []              = False
isPrefixOf (c1:cs1) (c2:cs2) = (c1 == c2) && cs1 `isPrefixOf` cs2

{-|
Check if list has list as prefix
-}
hasPrefix :: (Eq a) => [a] -> [a] -> Bool
hasPrefix [] _              = False
hasPrefix _ []              = True
hasPrefix (c1:cs1) (c2:cs2) = (c1 == c2) && cs2 `hasPrefix` cs1

{-|
Split at sub-sequence, excluding the sub-sequence from split results.

>>> splitAtSubSeqExcl [3..4] [1..10]
[[1,2],[5,6,7,8,9,10]]
-}
splitAtSubSeqExcl :: (Eq a) => [a] -> [a] -> [[a]]
splitAtSubSeqExcl _ [] = []
splitAtSubSeqExcl sub seq = splitAtSubSeqExcl' sub seq [] []
  where
    splitAtSubSeqExcl' _ [] subacc acc = reverse $ reverse subacc:acc
    splitAtSubSeqExcl' sub (x:xs) subacc acc
      | sub `isPrefixOf` (x:xs) = splitAtSubSeqExcl' sub (drop (length sub - 1) xs) [] (reverse subacc:acc)
      | otherwise               = splitAtSubSeqExcl' sub xs (x:subacc) acc

{-|
Split at element, exluding the element from split results

>>> splitAtExcl 3 [1..5] 
[[1,2],[4,5]]
-} 
splitAtExcl :: (Eq a) => a -> [a] -> [[a]]
splitAtExcl el = splitAtSubSeqExcl [el]

precedingFromN :: Int -> Int -> [a] -> [a]
precedingFromN n len ns'
            | len <= n = take len $ drop (n-len) ns'
            | otherwise = []

{-|
Generates all possible subsequences of length N that comprise of the elements in the list.

>>> subsequencesN 2 [1..5]
[[4,5],[3,5],[3,4],[2,5],[2,4],[2,3],[1,5],[1,4],[1,3],[1,2]]

-}
subsequencesN :: Int -> [a] -> [[a]]
subsequencesN n xs = if n > len then [] else subsequencesN' xs !! (len-n)
 where
   len = length xs
   subsequencesN' [] = [[[]]]
   subsequencesN' (x:xs) = zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
    where next = subsequencesN' xs

{-|
Creates subsegment windows out of a list

>>> windows 2 [1,2,3]
[[1,2],[2,3]]
-}
windows :: Int -> [a] -> [[a]]
windows l xs = map (\i -> take l $ drop i xs) [0..length xs-l]

{-|
Iteratively find an element that satisfies a condition from the output elements of a generator function

>>> iterFind (\s -> length s > 5) (++"a") "a"
"aaaaaa"

-}
iterFind :: (a -> Bool) -> (a -> a) -> a -> a
iterFind pred func = iterFind'
  where
    iterFind' x
      | pred x    = x
      | otherwise = iterFind' (func x)

