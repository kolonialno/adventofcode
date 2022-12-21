package main

import (
	"fmt"
	"strings"
)

type day21TreeNode struct {
	left, right *day21TreeNode
	value       *int
	op          *string
}

func (n *day21TreeNode) Num() int {
	if n.value != nil {
		return *n.value
	}
	left := n.left.Num()
	right := n.right.Num()
	switch *n.op {
	case "+":
		return left + right
	case "-":
		return left - right
	case "*":
		return left * right
	case "/":
		return left / right
	}
	panic("Unknown op " + *n.op)
}

func (a Advent) Day21() {
	lines := fileByLines("inputs/day21.txt")

	m := make(map[string]*day21TreeNode)
	for _, line := range lines {
		id := line[:4]
		n, ok := m[id]
		if !ok {
			n = new(day21TreeNode)
			m[id] = n
		}
		if strings.Index(line[6:], " ") == -1 {
			val := atoi[int](line[6:])
			n.value = &val
		} else {
			var left, op, right string
			fmt.Sscanf(line[6:], "%s %s %s", &left, &op, &right)
			for _, n := range []string{left, right} {
				if _, ok := m[n]; !ok {
					m[n] = new(day21TreeNode)
				}
			}
			n.left = m[left]
			n.op = &op
			n.right = m[right]
		}
	}

	fmt.Println("Part 1:", m["root"].Num())

	// Found the following by binary-searching by hand:
	*m["humn"].value = 3_740_214_169_961

	left, right := m["root"].left.Num(), m["root"].right.Num()
	if left != right {
		panic("left != right")
	}
	fmt.Println("Part 2:", *m["humn"].value)
}
