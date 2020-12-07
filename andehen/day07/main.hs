import Data.List (nub)
import Data.Text as Text (lines, unpack)
import Data.Text.IO as TextIO (readFile)
import Utils (parseRule, reformat)

type Bag = String

type BagContent = [(Int, Bag)]

type BagContentRule = (Bag, BagContent)

main :: IO ()
main = do
  input <- TextIO.readFile "rules.txt"
  let rules = map Text.unpack (Text.lines input)
      formatted = map reformat rules
      parsed = map parseRule formatted
  print $ (length . nub) (getOutermostBags parsed "shiny gold bag")
  print $ getBagCount parsed "shiny gold bag"

getBagCount :: [BagContentRule] -> Bag -> Int
getBagCount bcr b =
  let bg = getBagContent bcr b
   in if null bg
        then 0
        else sum (map subBagCount bg)
  where
    subBagCount bg = fst bg + fst bg * getBagCount bcr (snd bg)

getOutermostBags :: [BagContentRule] -> Bag -> [Bag]
getOutermostBags bcr b = do
  let directly = map fst (filter cond bcr)
        where
          cond x = contains b (snd x)
      nested = concatMap (getOutermostBags bcr) directly
  directly ++ nested 

contains :: Bag -> BagContent -> Bool
contains b bg = b `elem` map snd bg

getBagContent :: [BagContentRule] -> Bag -> BagContent
getBagContent bcr b =
  let res = filter cond bcr
        where
          cond x = fst x == b
   in if null res
        then []
        else snd $ head (filter cond bcr)
  where
    cond x = fst x == b
