package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type Bag struct {
	color    string
	quantity int
}

func importInput() map[string][]Bag {
	file, err := os.Open("7.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	bagList := map[string][]Bag{}
	r := regexp.MustCompile(`(\d*)(.*)\sbag`)

	for _, line := range s {
		splitLine := strings.Split(line, " contain ")
		values := []Bag{}
		commaSplit := strings.Split(splitLine[1], ",")
		for _, val := range commaSplit {
			res := r.FindStringSubmatch(strings.TrimSpace(val))

			intValue, err := strconv.Atoi(res[1])
			if err != nil {
				continue
			}

			bag := Bag{color: strings.TrimSpace(res[2]), quantity: intValue}
			values = append(values, bag)
		}
		keyRes := r.FindStringSubmatch(strings.TrimSpace(splitLine[0]))
		bagList[keyRes[2]] = values

	}
	return bagList
}

func searchShinyBagColorCount(bagMapping map[string][]Bag) int {
	count := 0

	for color, val := range bagMapping {
		if color == "shiny gold" {
			continue
		}
		count = count + dfs(bagMapping, val, Bag{color: color, quantity: 1})
	}

	return count
}

func dfs(bagMapping map[string][]Bag, bagList []Bag, bag Bag) int {

	if bag.color == "shiny gold" {
		return 1
	}
	for _, currentBag := range bagList {
		currentBagList := bagMapping[bag.color]
		if dfs(bagMapping, currentBagList, currentBag) == 1 {
			return 1
		}
	}
	return 0

}

func searchNumberOfBags(bagMapping map[string][]Bag) int {

	rootColor := "shiny gold"
	return dfsNumberOfBags(bagMapping, bagMapping[rootColor], Bag{color: rootColor, quantity: 1}) - 1
}

func dfsNumberOfBags(bagMapping map[string][]Bag, bagList []Bag, bag Bag) int {

	count := bag.quantity

	for _, currentBag := range bagList {
		currentBagList := bagMapping[currentBag.color]
		count = count + bag.quantity*dfsNumberOfBags(bagMapping, currentBagList, currentBag)
	}

	return count

}

func main() {
	input := importInput()
	result := searchShinyBagColorCount(input)
	fmt.Println(result)
	numberOfBags := searchNumberOfBags(input)
	fmt.Println(numberOfBags)
}
