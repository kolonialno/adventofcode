package main

import (
	"fmt"
	"strings"
)

func day03ParseNum(input string) (*int, int) {
	dx := 0
	foundDigit := false
	digitIndex := 0
	for ; digitIndex < len(input); digitIndex++ {
		if input[digitIndex] < '0' || input[digitIndex] > '9' {
			break
		}
		foundDigit = true
		digit := int(input[digitIndex] - '0')
		dx = dx*10 + digit
	}
	if !foundDigit {
		return nil, 0
	}
	return &dx, digitIndex
}

func (adv Advent) Day03() {
	inputs := fileByLines("inputs/day03.txt")
	sum := 0
	for _, input := range inputs {
		index := 0
		for {
			i := strings.Index(input[index:], "mul(")
			index += i
			if i < 0 {
				break
			}
			index += 4
			first, consumed := day03ParseNum(input[index:])
			index += consumed
			if first == nil {
				continue
			}
			comma := input[index : index+1]
			index++
			if comma != "," {
				continue
			}
			second, consumed := day03ParseNum(input[index:])
			index += consumed
			if second == nil {
				continue
			}
			rightParen := input[index : index+1]
			index++
			if rightParen != ")" {
				continue
			}
			product := *first * *second
			sum += product
		}
	}
	fmt.Printf("Part 1 %d\n", sum)
	sum = 0
	enabled := true
	for _, input := range inputs {
		index := 0
		for {
			i := strings.Index(input[index:], "mul(")
			ido := strings.Index(input[index:], "do()")
			idont := strings.Index(input[index:], "don't()")
			if ido >= 0 && ido < i && idont >= 0 && idont < i {
				if ido > idont {
					idont = -1
				} else {
					ido = -1
				}
			}
			if ido >= 0 && ido < i {
				enabled = true
			}
			if idont >= 0 && idont < i {
				enabled = false
			}
			index += i
			if i < 0 {
				break
			}
			index += 4
			first, consumed := day03ParseNum(input[index:])
			index += consumed
			if first == nil {
				continue
			}
			comma := input[index : index+1]
			index++
			if comma != "," {
				continue
			}
			second, consumed := day03ParseNum(input[index:])
			index += consumed
			if second == nil {
				continue
			}
			rightParen := input[index : index+1]
			index++
			if rightParen != ")" {
				continue
			}
			product := *first * *second
			if !enabled {
				continue
			}
			sum += product
		}
	}
	fmt.Printf("Part 2 %d\n", sum)
}
