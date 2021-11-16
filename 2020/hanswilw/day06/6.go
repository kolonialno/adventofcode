package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

func importInput() []string {
	file, err := os.Open("6.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	return s
}

func sumValues(input []string) int {

	countSum := 0

	currentForm := map[string]bool{}
	for _, val := range input {
		fmt.Println(val)
		if len(val) == 0 {
			if len(currentForm) > 0 {
				countSum = countSum + len(currentForm)
				currentForm = map[string]bool{}
			}
			continue
		}
		for _, char := range val {
			currentForm[string(char)] = true
		}
	}
	if len(currentForm) > 0 {
		countSum = countSum + len(currentForm)
	}

	return countSum
}

func sumValues2(input []string) int {

	countSum := 0

	currentForm := map[string]int{}
	groupSize := 0
	for _, val := range input {
		if len(val) == 0 {
			if len(currentForm) > 0 {
				for _, count := range currentForm {
					if count == groupSize {
						countSum++
					}
				}
				currentForm = map[string]int{}
				groupSize = 0
			}
			continue
		}
		for _, char := range val {
			currentForm[string(char)]++
		}
		groupSize++
	}
	for _, count := range currentForm {
		if count == groupSize {
			countSum++
		}
	}

	return countSum
}

func main() {
	input := importInput()
	result := sumValues(input)
	fmt.Println(result)
	result2 := sumValues2(input)
	fmt.Println(result2)
}
