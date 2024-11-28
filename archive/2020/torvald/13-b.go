package main

import (
	"fmt"
	"strconv"
	"strings"
	utils "torvald/libs"
)

func main() {

	lines, _ := utils.ReadLinesStrings("input/13.txt")

	busesString := strings.Split(lines[1], ",")
	busLen := len(busesString)
	buses := make(map[int]int)

	for index, bus := range busesString {

		if bus == "x" {
			continue
		}
		busInt, _ := strconv.Atoi(bus)
		buses[busInt] = index
	}

	departureSeen := make(map[int]bool)

	t := 0

	tDelta := busLen

	for true {

		found := true

		for busInt, offset := range buses {

			if ((t + offset) % busInt) != 0 {
				found = false
				break
			}

			departureSeen[busInt] = true

			// 1. First time we see a departue, we start using that that busInt
			// as tDelta, we know there is nothing in between.
			// 2. Next time we see two departue in the same "round", we know know
			// the next possible solution is at least t =  busInt1 * busInt2 away
			// 3. And so on, with bigger and bigger deltas as we find rounds
			// with more depatures in it
			newDelta := 1
			for key := range departureSeen {
				newDelta *= key
			}
			tDelta = newDelta
		}

		if found {
			fmt.Println("All departures found in round", t)
			break
		}

		t += tDelta
	}
}
