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
	file, err := os.Open("9.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	inputList := []int{}
	for _, val := range s {
		integerValue, _ := strconv.Atoi(val)
		inputList = append(inputList, integerValue)
	}
	return inputList
}

func slidingWindowDetectError(inputList []int, k int) int {

	validNumber := 0
	left := 0
	right := k

	for right < len(inputList) {
		target := inputList[right]

		slice := inputList[left:right]
		if !sumOfTwoElements(slice, target) {
			return inputList[right]
		}
		left = left + 1
		right = right + 1
	}

	return validNumber
}

func sumOfTwoElements(listSlice []int, target int) bool {
	seen := map[int]bool{}
	for _, val := range listSlice {
		if seen[target-val] {
			return true
		}
		seen[val] = true
	}
	return false
}

func slidingWindowSumInvalid(inputList []int, target int, k int) int {

	valueList := []int{}
	sum := 0

	left := 0
	right := 1

	for right < len(inputList) {
		if sum == target {
			break
		}

		if sum < target {
			val := inputList[right]
			valueList = append(valueList, val)
			sum = sum + val
			right++
		} else if sum > target {
			val := valueList[0]
			valueList = valueList[1:]
			sum = sum - val
			left++
		}
	}

	return sumMinMax(valueList)
}

func sumMinMax(slice []int) int {
	min := slice[0]
	max := slice[0]
	for _, val := range slice {
		if val == 0 || val < min {
			min = val
		}
		if val == 0 || val > max {
			max = val
		}
	}
	return min + max
}

func main() {
	input := importInput()
	result := slidingWindowDetectError(input, 25)
	fmt.Println(result) //20874512
	result2 := slidingWindowSumInvalid(input, result, 25)
	fmt.Println(result2)
}
