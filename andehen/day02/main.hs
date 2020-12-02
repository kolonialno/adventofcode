{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO


main :: IO ()
main = do
    passwords <- fmap Text.lines (TextIO.readFile "passwords.txt")

    let password_lists = map passwordsToList passwords
        valid_passwords_part_one = map validPasswordPartOne password_lists
        result_part_one = length $ filter (==True) valid_passwords_part_one
    
    print $ "Part one: " ++ show result_part_one

    let valid_passwords_part_two = map validPasswordPartTwo password_lists
        result_part_two = length $ filter (==True) valid_passwords_part_two

    print $ "Part two: " ++ show result_part_two

validPasswordPartOne :: [Text.Text] -> Bool
validPasswordPartOne pl = 
    let min = textToInt (pl !! 0)
        max = textToInt (pl !! 1)
        pass = pl !! 3
        char = (Text.unpack (pl !! 2)) !! 0
    in countLetters pass char <? (min, max)


validPasswordPartTwo :: [Text.Text] -> Bool
validPasswordPartTwo pl = 
    let p1 = (textToInt (pl !! 0)) - 1
        p2 = (textToInt (pl !! 1)) - 1
        pass = pl !! 3
        p1char = (Text.unpack pass) !! p1
        p2char = (Text.unpack pass) !! p2
        char = (Text.unpack (pl !! 2)) !! 0
    in
        if (p1char == char) && (p2char /= char) then True
        else if (p1char /= char) && (p2char == char) then True
        else False

-- Split text into lists with information about password
passwordsToList :: Text.Text -> [Text.Text]
passwordsToList p = 
    filter ("" /=)
        (concat (map (Text.splitOn "-")
            (concat (map (Text.splitOn " ")
                (Text.splitOn ":" p)
            ))
        ))

countLetters :: Text.Text -> Char -> Int
countLetters txt c = Text.length $ Text.filter (== c) txt

-- Function that returns wheter a value is within a range or not
(<?) :: Ord a => a -> (a,a) -> Bool
(<?) x (min, max) = x >= min && x <= max

textToInt :: Text.Text -> Int
textToInt a = read $ Text.unpack a :: Int
