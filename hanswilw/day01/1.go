package main

import (
	"fmt"
	"os"
	"log"
	"io/ioutil"
	"strconv"
	"strings"
)

func import_input() []int {
	file, err := os.Open("1.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	input_list := make([]int, 0, len(s))

	for _, i := range s {
		j, err := strconv.Atoi(i)
		if err != nil {
			log.Fatal(err)
		}
		input_list = append(input_list, j)
	}
	return input_list
}


func twoproduct(entries []int) int {
	target := 2020
	seen := make(map[int] bool)

	for _, entry := range entries {
		if seen[target - entry] {
			fmt.Println("Two product:", entry, target - entry)
			return entry * (target - entry)
		}
		seen[entry] = true
	}

	return -1
}

func threeproduct(entries []int) int {
	target := 2020

	for i, i_val := range entries {
		j := i + 1
		seen := make(map[int] bool)
		for j < len(entries) {
			j_val := entries[j]

			if seen[target - i_val - j_val] {
				fmt.Println("Three product:", i_val, j_val, (target - j_val - i_val))
				return i_val * j_val * (target - j_val - i_val)
			}
			seen[j_val] = true
			j++
		}
	}

	return -1
}

func main() {
	input := import_input()
	result := twoproduct(input)
	fmt.Println(result)
	result2 := threeproduct(input)
	fmt.Println(result2)
}