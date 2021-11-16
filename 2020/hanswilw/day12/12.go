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
	direction byte
	unit      int
}

func importInput() []Instruction {
	file, err := os.Open("12.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	instructionList := make([]Instruction, 0, len(s))
	for _, val := range s {
		direction, unit := val[0], val[1:]
		unitInteger, _ := strconv.Atoi(unit)
		instructionList = append(instructionList, Instruction{direction: direction, unit: unitInteger})

	}
	return instructionList
}

func partOne(instructions []Instruction) int {

	direction := 0
	positionX := 0
	positionY := 0
	for _, val := range instructions {
		if val.direction == 'F' {
			if direction == 0 {
				positionX += val.unit
			} else if direction == 90 {
				positionY += val.unit
			} else if direction == 180 {
				positionX -= val.unit
			} else if direction == 270 {
				positionY -= val.unit
			}
		} else if val.direction == 'N' {
			positionY -= val.unit
		} else if val.direction == 'S' {
			positionY += val.unit
		} else if val.direction == 'E' {
			positionX += val.unit
		} else if val.direction == 'W' {
			positionX -= val.unit
		} else if val.direction == 'L' {
			direction = (direction - val.unit)
			direction = (direction%360 + 360) % 360
		} else if val.direction == 'R' {
			direction = (direction + val.unit)
			direction = (direction%360 + 360) % 360
		}
	}

	return positionX + positionY
}
func partTwo(instructions []Instruction) int {

	positionX := 0
	positionY := 0

	waypointX := 10
	waypointY := -1
	for _, val := range instructions {
		if val.direction == 'F' {
			for i := 0; i < val.unit; i++ {
				positionX += waypointX
				positionY += waypointY
			}
		} else if val.direction == 'N' {
			waypointY -= val.unit
		} else if val.direction == 'S' {
			waypointY += val.unit
		} else if val.direction == 'E' {
			waypointX += val.unit
		} else if val.direction == 'W' {
			waypointX -= val.unit
		} else if val.direction == 'L' {
			if val.unit == 90 {
				waypointX, waypointY = waypointY, -waypointX
			} else if val.unit == 180 {
				waypointX, waypointY = -waypointX, -waypointY
			} else if val.unit == 270 {
				waypointX, waypointY = -waypointY, waypointX
			}
		} else if val.direction == 'R' {
			if val.unit == 90 {
				waypointX, waypointY = -waypointY, waypointX
			} else if val.unit == 180 {
				waypointX, waypointY = -waypointX, -waypointY
			} else if val.unit == 270 {
				waypointX, waypointY = waypointY, -waypointX
			}
		}
	}

	return positionX + positionY
}

func main() {
	input := importInput()
	result := partOne(input)
	fmt.Println(result)
	result2 := partTwo(input)
	fmt.Println(result2)
}
