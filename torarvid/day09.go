package main

import (
	"fmt"
	"strings"
)

type D9Map string

func (m D9Map) String() string {
	builder := strings.Builder{}
	isSpace := false
	id := 0
	for _, char := range m {
		num := int(char) - int('0')
		if isSpace {
			builder.WriteString(strings.Repeat(".", num))
		} else {
			builder.WriteString(strings.Repeat(fmt.Sprintf("%d", id), num))
			id++
		}
		isSpace = !isSpace
	}
	return builder.String()
}

func (m D9Map) Checksum1() int {
	head, tail := 0, len(m)-1
	tailNum := int(m[tail]) - int('0')
	isSpace := false
	offset := 0
	sum := 0
	for head <= tail {
		num := int(m[head]) - int('0')
		if head == tail {
			num = Min(num, tailNum)
		}
		for i := 0; i < num; i++ {
			if isSpace {
				if tailNum == 0 {
					tail -= 2
					if tail < head {
						break
					}
					tailNum = int(m[tail]) - int('0')
				}
				tailNum--
				sum += offset * tail / 2
			} else {
				sum += offset * head / 2
			}
			offset++
		}
		isSpace = !isSpace
		head++
	}
	return sum
}

func (adv Advent) Day09() {
	input := D9Map(fileByLines("inputs/day09_example.txt")[0])
	fmt.Printf("Part 1: %d\n", input.Checksum1())
}
