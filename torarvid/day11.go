package main

import (
	"fmt"
	"sort"
	"strings"
)

type day11Monkey struct {
	inspections  int
	items        []int64
	op           func(int64) int64
	testAndThrow func(int64) int
}

func day11Parse(lines []string) []day11Monkey {
	monkeys := make([]day11Monkey, 0)
	var theCruxOhYesTheCruxOfTheTask int64 = 1
	for i := 1; i < len(lines); i += 7 {
		monkey := day11Monkey{}
		itemsStr := strings.Split(lines[i][18:], ", ")
		monkey.items = Map(itemsStr, atoi[int64])

		var op string
		var opArg string
		fmt.Sscanf(lines[i+1], "  Operation: new = old %s %s", &op, &opArg)
		monkey.op = func(old int64) int64 {
			x := old
			if opArg != "old" {
				x = atoi[int64](opArg)
			}
			var result int64
			switch op {
			case "*":
				result = old * x
			case "+":
				result = old + x
			}
			return result % theCruxOhYesTheCruxOfTheTask
		}

		var divBy int64
		fmt.Sscanf(lines[i+2], "  Test: divisible by %d", &divBy)
		theCruxOhYesTheCruxOfTheTask *= divBy

		var ifTrue, ifFalse int
		fmt.Sscanf(lines[i+3], "    If true: throw to monkey %d", &ifTrue)
		fmt.Sscanf(lines[i+4], "    If false: throw to monkey %d", &ifFalse)
		monkey.testAndThrow = func(worry int64) int {
			if worry%divBy == 0 {
				return ifTrue
			}
			return ifFalse
		}
		monkeys = append(monkeys, monkey)
	}
	return monkeys
}

func day11Solve(monkeys []day11Monkey, rounds int, divByThree bool) int64 {
	results := make([]int, len(monkeys))
	for round := 1; round <= rounds; round++ {
		for i := range monkeys {
			for len(monkeys[i].items) > 0 {
				item := monkeys[i].items[0]
				newWorry := monkeys[i].op(item)
				monkeys[i].inspections++
				if divByThree {
					newWorry /= 3
				}
				destMonkey := monkeys[i].testAndThrow(newWorry)
				monkeys[i].items = monkeys[i].items[1:]
				monkeys[destMonkey].items = append(monkeys[destMonkey].items, newWorry)
			}
			results[i] = monkeys[i].inspections
		}
	}
	sort.Sort(sort.Reverse(sort.IntSlice(results)))
	return int64(results[0]) * int64(results[1])
}

func (a Advent) Day11() {
	lines := fileByLines("inputs/day11.txt")

	fmt.Println("Part 1:", day11Solve(day11Parse(lines), 20, true))
	fmt.Println("Part 2:", day11Solve(day11Parse(lines), 10000, false))
}
