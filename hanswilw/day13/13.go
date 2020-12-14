package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"
)

type Shuttle struct {
	timestamp  int
	departList []int
}

func importInput() Shuttle {
	file, err := os.Open("13.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	timestamp, _ := strconv.Atoi(s[0])
	departInput := strings.Split(s[1], ",")
	departList := []int{}
	for _, val := range departInput {
		if val != "x" {
			valInteger, _ := strconv.Atoi(val)
			departList = append(departList, valInteger)
		}

	}
	shuttle := Shuttle{timestamp: timestamp, departList: departList}
	return shuttle
}

func partOne(shuttle Shuttle) int {

	smallest := shuttle.departList[0]
	busID := shuttle.departList[0]

	for _, val := range shuttle.departList[1:] {
		remainder := shuttle.timestamp % val
		if (val - remainder) < smallest {
			smallest = val - remainder
			busID = val
		}
	}

	return busID * smallest
}

func importInput2() []string {
	file, err := os.Open("13.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	departInput := strings.Split(s[1], ",")
	departList := []string{}
	for _, val := range departInput {
		departList = append(departList, val)

	}
	return departList
}

func gcd(x int, y int) int {
	for y != 0 {
		t := y
		y = x % y
		x = t
	}
	return x
}

func lcm(x int, y int) int {
	return (x * y) / gcd(x, y)
}

func partTwo(departList []string) int {
	timestamp := 0
	step, _ := strconv.Atoi(departList[0])

	for i, val := range departList[1:] {
		if val == "x" {
			continue
		}
		integerValue, _ := strconv.Atoi(val)

		for (timestamp+i+1)%integerValue != 0 {
			timestamp += step
		}
		step = lcm(step, integerValue)
	}
	return timestamp
}

func main() {
	input := importInput()
	result := partOne(input)
	fmt.Println(result)
	input2 := importInput2()
	result2 := partTwo(input2)
	fmt.Println(result2)
}
