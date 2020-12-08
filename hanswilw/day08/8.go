package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"
)

type Instruction struct {
	operation string
	argument  int
}

func importInput() []Instruction {
	file, err := os.Open("8.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	instructionList := []Instruction{}
	for _, val := range s {
		instruction := strings.Split(val, " ")
		argument, _ := strconv.Atoi(instruction[1])
		instructionList = append(instructionList, Instruction{operation: instruction[0], argument: argument})
	}
	return instructionList
}

func findValueBeforeLoop(instructionList []Instruction) int {
	value := 0
	seen := map[int]bool{}

	i := 0
	for i < len(instructionList) {
		instruction := instructionList[i]
		if seen[i] == true {
			return value
		}

		seen[i] = true
		if instruction.operation == "acc" {
			value = value + instruction.argument
			i = i + 1
		} else if instruction.operation == "jmp" {
			i = i + instruction.argument
		} else {
			i = i + 1
		}

	}

	return value
}

func fixBootCode(instructionList []Instruction) int {
	seen := map[int]bool{}
	return verifyBootCode(instructionList, 0, seen, false, 0)
}

func verifyBootCode(instructionList []Instruction, i int, seen map[int]bool, flipped bool, value int) int {

	for i < len(instructionList) {
		instruction := instructionList[i]
		if seen[i] == true {
			return 0
		}

		seenCopy := map[int]bool{}
		for key, val := range seen {
			seenCopy[key] = val
		}
		seen[i] = true
		if instruction.operation == "acc" {
			value = value + instruction.argument
			i = i + 1
		} else if instruction.operation == "jmp" {
			if !flipped {
				instructionList[i] = Instruction{operation: "nop", argument: instruction.argument}
				verifiedValue := verifyBootCode(instructionList, i, seenCopy, true, value)
				if verifiedValue > 0 {
					return verifiedValue
				}
			}
			i = i + instruction.argument
		} else {
			if !flipped {
				instructionList[i] = Instruction{operation: "jmp", argument: instruction.argument}
				verifiedValue := verifyBootCode(instructionList, i, seenCopy, true, value)
				if verifiedValue > 0 {
					return verifiedValue
				}
			}
			i = i + 1
		}

	}

	return value
}

func main() {
	input := importInput()
	result := findValueBeforeLoop(input)
	fmt.Println(result)
	result2 := fixBootCode(input)
	fmt.Println(result2)

}
