package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type answer struct {
	partOne int
	partTwo int
}

type group struct {
	min int
	max int
}

func groupFromString(groupString string) group {

	groupBounds := strings.Split(groupString, "-")
	min, _ := strconv.Atoi(groupBounds[0])
	max, _ := strconv.Atoi(groupBounds[1])
	return group{min: min, max: max}
}

func groupsFromString(line string) (group, group) {

	groupStrings := strings.Split(line, ",")
	return groupFromString(groupStrings[0]), groupFromString(groupStrings[1])

}

// Fully contained
func solvePartOne(group1 group, group2 group) int {

	if group1.min == group2.min {
		return 1
	}

	var outerGroup *group
	var innerGroup *group
	if group1.min < group2.min {
		outerGroup, innerGroup = &group1, &group2
	} else {
		outerGroup, innerGroup = &group2, &group1
	}

	if outerGroup.max >= innerGroup.max {
		return 1
	} else {
		return 0
	}
}

// Partially contained
func solvePartTwo(group1 group, group2 group) int {

	if group1.min == group2.min {
		return 1
	}
	if group1.max == group2.max {
		return 1
	}

	var leftGroup *group
	var rightGroup *group
	if group1.min < group2.min {
		leftGroup, rightGroup = &group1, &group2
	} else {
		leftGroup, rightGroup = &group2, &group1
	}

	if leftGroup.max >= rightGroup.min {
		return 1
	} else {
		return 0
	}
}

func solve(filePath string) answer {

	input, _ := os.Open(filePath)
	defer input.Close()

	partOneSum, partTwoSum := 0, 0
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		group1, group2 := groupsFromString(scanner.Text())
		partOneSum += solvePartOne(group1, group2)
		partTwoSum += solvePartTwo(group1, group2)
	}

	return answer{partOne: partOneSum, partTwo: partTwoSum}

}

func main() {

	filePath := "day04.txt"
	fmt.Println(solve(filePath))

}
