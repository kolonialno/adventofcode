package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"
)

type Program struct {
	mask               string
	memoryInstructions []MemoryInstruction
}

type MemoryInstruction struct {
	address    int
	writeValue int
}

func importInput() []int {
	file, err := os.Open("15.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), ",")

	integerList := make([]int, 0, len(s))
	for _, char := range s {
		integerChar, _ := strconv.Atoi(char)
		integerList = append(integerList, integerChar)
	}

	return integerList
}

func scan(numbers []int, target int) int {

	spoken := map[int]int{}

	for i, num := range numbers[:len(numbers)-1] {
		spoken[num] = i + 1
	}

	turn := len(spoken) + 1

	prevNumber := numbers[len(numbers)-2]
	number := numbers[len(numbers)-1]
	newNumber := 0

	for turn < target {
		if number == prevNumber {
			newNumber = 1
		} else if spoken[number] == turn-1 {
			newNumber = 1
		} else if spoken[number] == 0 {
			newNumber = 0
		} else if spoken[number] > 0 {
			newNumber = turn - spoken[number]
		}

		spoken[number] = turn
		turn++
		prevNumber = number
		number = newNumber

	}

	return newNumber

}

func main() {
	input := importInput()
	result := scan(input, 2020)
	fmt.Println(result)
	result2 := scan(input, 30000000)
	fmt.Println(result2)
}
