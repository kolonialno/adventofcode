package main

import (
	"fmt"
	"strconv"
	"strings"
	utils "torvald/libs"
)

func main() {
	lines, _ := utils.ReadLinesStrings("input/13.txt")

	target, _ := strconv.Atoi(lines[0])
	busesString := strings.Split(lines[1], ",")
	buses := make(map[int]bool)

	lowest := target
	bestBus := 0

	for _, bus := range busesString {
		if bus == "x" {
			continue
		}

		busInt, _ := strconv.Atoi(bus)
		buses[busInt] = true

		timeToNext := (target/busInt*busInt + busInt) - target

		if timeToNext < lowest {
			lowest = timeToNext
			bestBus = busInt
		}

	}
	fmt.Println(bestBus * lowest)

}
