package main

import (
	"fmt"
	"sort"
)

type day13Packet struct {
	n []any
}

func day13Cmp(lhs, rhs any) int {
	li, liok := lhs.(int)
	ri, riok := rhs.(int)
	if liok && riok {
		if li < ri {
			return -1
		} else if li > ri {
			return 1
		}
		return 0
	}
	if liok {
		return day13Cmp(day13Packet{[]any{lhs}}, rhs)
	}
	if riok {
		return day13Cmp(lhs, day13Packet{[]any{rhs}})
	}
	lp, lpok := lhs.(day13Packet)
	rp, rpok := rhs.(day13Packet)
	if lpok && rpok {
		for i := range lp.n {
			if i >= len(rp.n) {
				return 1
			}
			cmp := day13Cmp(lp.n[i], rp.n[i])
			if cmp == 0 {
				continue
			}
			return cmp
		}
		if len(lp.n) < len(rp.n) {
			return -1
		}
	}
	return 0
}

func day13NewPacket(input string) day13Packet {
	var parse func(string) (int, day13Packet)
	parse = func(input string) (int, day13Packet) {
		p := day13Packet{}
		p.n = make([]any, 0)

		var num *int
		for i := 0; i < len(input); i++ {
			c := input[i]
			if c == '[' {
				n, child := parse(input[i+1:])
				p.n = append(p.n, child)
				i += n
			} else if c == ']' {
				if num != nil {
					p.n = append(p.n, *num)
					num = nil
				}
				return i + 1, p
			} else if c == ',' {
				if num != nil {
					p.n = append(p.n, *num)
					num = nil
				}
				continue
			} else {
				if num == nil {
					n := int(c - '0')
					num = &n
				} else {
					*num = *num*10 + int(c-'0')
				}
			}
		}

		return 0, p
	}
	_, p := parse(input)
	return p
}

func (a Advent) Day13() {
	lines := fileByLines("inputs/day13.txt")
	packets := make([]day13Packet, 0)
	for _, line := range lines {
		if line == "" {
			continue
		}
		packets = append(packets, day13NewPacket(line))
	}

	sum := 0
	for i := 0; i < len(packets); i += 2 {
		if day13Cmp(packets[i], packets[i+1]) == -1 {
			sum += i/2 + 1
		}
	}
	fmt.Println("Part 1:", sum)

	special1, special2 := day13NewPacket("[[2]]"), day13NewPacket("[[6]]")
	packets = append(packets, special1, special2)
	sort.Slice(packets, func(i, j int) bool {
		return day13Cmp(packets[i], packets[j]) == -1
	})
	result := 1
	for i, p := range packets {
		if day13Cmp(p, special1) == 0 || day13Cmp(p, special2) == 0 {
			result *= i + 1
		}
	}

	fmt.Println("Part 2:", result)
}
