package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"
)

func importInput() map[int]bool {
	file, err := os.Open("10.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	inputMap := map[int]bool{}
	for _, val := range s {
		integerValue, _ := strconv.Atoi(val)
		inputMap[integerValue] = true
	}
	return inputMap
}

func getJoltDiffsMultiplied(inputMap map[int]bool) int {

	joltDifferences := map[int]int{}
	dfs(inputMap, 0, 0, joltDifferences)
	return joltDifferences[1] * (joltDifferences[3] + 1)
}

func dfs(inputMap map[int]bool, targetJolt int, depth int, joltDifferences map[int]int) int {

	currentDepth := depth
	for i := targetJolt + 1; i <= targetJolt+3; i++ {
		if !inputMap[i] {
			continue
		}
		value := dfs(inputMap, i, depth+1, joltDifferences)
		if value > currentDepth {
			joltDifferences[i-targetJolt]++
			currentDepth = value
		}

		// Return early if all adapters are used
		if currentDepth == len(inputMap) {
			return currentDepth
		}
	}
	return currentDepth

}

func getDistinctPaths(inputMap map[int]bool) int {

	maxJolt := 0
	for adapterJolt := range inputMap {
		if adapterJolt > maxJolt {
			maxJolt = adapterJolt
		}
	}
	memo := map[int]int{}
	result := dfsPaths(inputMap, 0, maxJolt+3, memo)
	return result
}

func dfsPaths(inputMap map[int]bool, adapterJolt int, targetJolt int, memo map[int]int) int {
	if val, ok := memo[adapterJolt]; ok {
		return val
	}
	pathCount := 0

	for i := adapterJolt + 1; i <= adapterJolt+3; i++ {
		if !inputMap[i] {
			continue
		}
		if i+3 == targetJolt {
			return pathCount + 1
		}

		pathCount = pathCount + dfsPaths(inputMap, i, targetJolt, memo)

	}
	memo[adapterJolt] = pathCount
	return pathCount

}

func main() {
	input := importInput()
	result := getJoltDiffsMultiplied(input)
	fmt.Println(result)
	result2 := getDistinctPaths(input)
	fmt.Println(result2)
}
