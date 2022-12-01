package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

type results struct {
	partOne int
	partTwo int
}

func solve(filePath string) results {

	input, _ := os.Open(filePath)
	defer input.Close()

	elvesCalories := []int{}
	currentElfCalories := 0

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		currentCalories, err := strconv.Atoi(scanner.Text())
		if err != nil {
			elvesCalories = append(elvesCalories, currentElfCalories)
			currentElfCalories = 0
		} else {
			currentElfCalories += currentCalories
		}
	}
	elvesCalories = append(elvesCalories, currentElfCalories)

	sort.Slice(elvesCalories, func(i, j int) bool {
		return elvesCalories[i] > elvesCalories[j]
	})

	top3ElvesCalorieSum := 0
	for i := 0; i < 3; i++ {
		top3ElvesCalorieSum += elvesCalories[i]
	}

	return results{partOne: elvesCalories[0], partTwo: top3ElvesCalorieSum}

}

func main() {

	filePath := "day01Calories.txt"
	fmt.Println(solve(filePath))

}
