package main

import (
	"fmt"
	"regexp"
	"strings"
	utils "torvald/libs"
)

func main() {

	bags := make(map[string][]string)
	lines, _ := utils.ReadLinesStrings("input/7.txt")

	for _, line := range lines {

		lineSplit := strings.Split(line, " bags contain ")
		parentBag := lineSplit[0]
		innerBagsString := strings.Split(lineSplit[1], ",")

		for _, innerBagString := range innerBagsString {

			pattern := regexp.MustCompile(`([0-9]+) ([a-z]*) ([a-z]*) bag`)
			match := pattern.FindStringSubmatch(innerBagString)
			if match == nil {
				continue
			}

			innerBag := match[2] + " " + match[3]
			bags[innerBag] = append(bags[innerBag], parentBag)
		}
	}

	myBag := "shiny gold"
	bagsSeen := make(map[string]int)
	var find func(bag string)

	find = func(bag string) {
		for _, parentBag := range bags[bag] {
			bagsSeen[parentBag]++
			find(parentBag)
		}
	}

	find(myBag)

	fmt.Println(len(bagsSeen))
}
