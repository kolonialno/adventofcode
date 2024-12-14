package main

import (
	"fmt"
	"strings"
)

type D11Elem struct {
	val  int
	next *D11Elem
}

func (e *D11Elem) MaybeSplit() bool {
	strNum := fmt.Sprintf("%d", e.val)
	numDigits := len(strNum)
	if numDigits%2 == 1 {
		return false
	}
	leftnum := Atoi[int](strNum[0 : numDigits/2])
	rightnum := Atoi[int](strNum[numDigits/2:])

	newElem := D11Elem{val: rightnum, next: e.next}
	e.val = leftnum
	e.next = &newElem
	return true
}

func (e *D11Elem) Blink() {
	current := e
	for current != nil {
		next := current.next
		if current.val == 0 {
			current.val = 1
		} else {
			if !current.MaybeSplit() {
				current.val *= 2024
			}
		}
		current = next
	}
}

func (e D11Elem) Count() int {
	count := 1
	n := e.next
	for n != nil {
		count++
		n = n.next
	}
	return count
}

func (adv Advent) Day11() {
	input := fileByLines("inputs/day11.txt")[0]
	var head *D11Elem
	var prev *D11Elem
	for _, strnum := range strings.Split(input, " ") {
		val := Atoi[int](strnum)
		e := D11Elem{val: val, next: nil}
		if head == nil {
			head = &e
		}
		if prev != nil {
			prev.next = &e
		}
		prev = &e
	}
	for i := 0; i < 25; i++ {
		head.Blink()
	}
	fmt.Printf("Part 1: %d\n", head.Count())
}
