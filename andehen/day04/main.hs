{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO
import qualified Data.List as List
import Text.Replace


main = do
    passports_input <- TextIO.readFile "passports.txt"
    let passports = map Text.unpack (Text.splitOn "\n\n" passports_input)
        rule = Replace "\n" " "
        parsed = map (replaceWithList [rule]) passports
        is_valid_passport = map isValidPassport parsed
    print $ length (filter (==True) is_valid_passport)

required_codes = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValidPassport :: String -> Bool
isValidPassport passport = 
    let has_code_funcs = map List.isInfixOf required_codes
    in
    all (==True) (map ($ passport) has_code_funcs)
