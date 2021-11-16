package main

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
	utils "torvald/libs"
)

func main() {

	bags := make(map[string]map[string]int)
	lines, _ := utils.ReadLinesStrings("input/7.txt")

	for _, line := range lines {

		lineSplit := strings.Split(line, " bags contain ")
		parentBag := lineSplit[0]
		innerBagsString := strings.Split(lineSplit[1], ",")

		bags[parentBag] = make(map[string]int)

		for _, innerBagString := range innerBagsString {

			pattern := regexp.MustCompile(`([0-9]+) ([a-z]*) ([a-z]*) bag`)
			match := pattern.FindStringSubmatch(innerBagString)
			if match == nil {
				continue
			}

			innerBag := match[2] + " " + match[3]
			number, _ := strconv.Atoi(match[1])

			bags[parentBag][innerBag] = number
		}
	}

	var find func(bag string) int
	myBag := "shiny gold"

	find = func(bag string) int {
		if len(bags[bag]) == 0 {
			return 1
		}
		count := 1
		for innerBag, number := range bags[bag] {
			count += number * find(innerBag)
		}
		return count
	}

	fmt.Println(find(myBag) - 1)

}
