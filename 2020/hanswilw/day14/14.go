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

type Program struct {
	mask               string
	memoryInstructions []MemoryInstruction
}

type MemoryInstruction struct {
	address    int
	writeValue int
}

func importInput() []Program {
	file, err := os.Open("14.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	r := regexp.MustCompile(`mem\[(\d*)\] = (\d*)`)
	s := strings.Split(string(b), "\n")
	programList := []Program{}

	currentProgram := Program{mask: s[0][7:], memoryInstructions: []MemoryInstruction{}}
	for _, val := range s[1:] {
		if val[:4] == "mask" {
			programList = append(programList, currentProgram)
			currentProgram = Program{mask: val[7:], memoryInstructions: []MemoryInstruction{}}
			continue
		}
		res := r.FindStringSubmatch(val)
		address, _ := strconv.Atoi(res[1])
		writeValue, _ := strconv.Atoi(res[2])
		currentProgram.memoryInstructions = append(currentProgram.memoryInstructions, MemoryInstruction{address: address, writeValue: writeValue})
	}
	programList = append(programList, currentProgram)
	return programList
}

func partOne(programList []Program) int {

	mem := map[int]int{}

	for _, program := range programList {

		for _, memoryInstruction := range program.memoryInstructions {

			calculatedValue := memoryInstruction.writeValue
			for i := len(program.mask) - 1; i >= 0; i-- {
				j := len(program.mask) - 1 - i
				val := program.mask[i]
				if val == 'X' {
					continue
				}

				if val == '0' {
					calculatedValue = calculatedValue & ^(1 << (j))
				}

				if val == '1' {
					calculatedValue = calculatedValue | 1<<(j)
				}
			}

			mem[memoryInstruction.address] = calculatedValue
		}

	}
	sum := 0

	for _, val := range mem {
		sum += val
	}
	return sum
}

func getMemoryAddresses(address int, mask string) map[int]bool {
	memoryAddresses := map[int]bool{}

	for i := 0; i < len(mask); i++ {
		j := len(mask) - 1 - i
		val := mask[i]
		if val == '1' {
			address = address | 1<<(j)
		}
	}
	memoryAddresses[address] = true

	index := 0
	for i := index; i < len(mask); i++ {
		val := mask[i]
		j := len(mask) - 1 - i
		if val == 'X' {
			for key := range memoryAddresses {
				memoryAddresses[key & ^(1<<j)] = true
				memoryAddresses[key|(1<<j)] = true
			}
		}
	}

	return memoryAddresses
}

func partTwo(programList []Program) int {

	mem := map[int]int{}

	for _, program := range programList {

		for _, memoryInstruction := range program.memoryInstructions {
			memoryAddresses := getMemoryAddresses(memoryInstruction.address, program.mask)

			for key := range memoryAddresses {
				mem[key] = memoryInstruction.writeValue
			}
		}

	}

	sum := 0
	for _, val := range mem {
		sum += val
	}
	return sum
}

func main() {
	input := importInput()
	result := partOne(input)
	fmt.Println(result)
	result2 := partTwo(input)
	fmt.Println(result2)
}
