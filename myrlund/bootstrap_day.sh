#!/bin/bash
set -e

printf -v folder "ep%02g" $1

data=$(curl -sf -b session=$SESSION_KEY https://adventofcode.com/2022/day/$1/input)
cp -pr scaffold $folder
sed -i "" "s/EPISODE/$folder/g" $folder/Cargo.toml
echo $data > $folder/input.txt

open -a "Visual Studio Code" $folder
echo GOGOGO