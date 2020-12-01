package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"
)

func importInput() []int {
	file, err := os.Open("1.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	inputList := make([]int, 0, len(s))

	for _, i := range s {
		j, err := strconv.Atoi(i)
		if err != nil {
			log.Fatal(err)
		}
		inputList = append(inputList, j)
	}
	return inputList
}

func twoproduct(entries []int) int {
	target := 2020
	seen := make(map[int]bool)

	for _, entry := range entries {
		if seen[target-entry] {
			fmt.Println("Two product:", entry, target-entry)
			return entry * (target - entry)
		}
		seen[entry] = true
	}

	return -1
}

func threeproduct(entries []int) int {
	target := 2020

	for i, iVal := range entries {
		seen := make(map[int]bool)
		for j := i + 1; j < len(entries); j++ {
			jVal := entries[j]

			if seen[target-iVal-jVal] {
				fmt.Println("Three product:", iVal, jVal, (target - jVal - iVal))
				return iVal * jVal * (target - jVal - iVal)
			}
			seen[jVal] = true
		}
	}

	return -1
}

func main() {
	input := importInput()
	result := twoproduct(input)
	fmt.Println(result)
	result2 := threeproduct(input)
	fmt.Println(result2)
}
