package main

import (
	"fmt"
	"sort"
	"strings"
)

func (a Advent) Day07() {
	input := fileByLines("inputs/day07.txt")
	sizes := make(map[string]int)
	path := make([]string, 0)
	for _, line := range input {
		if strings.HasPrefix(line, "$") {
			if strings.HasPrefix(line, "$ cd ") {
				var dir string
				fmt.Sscanf(line, "$ cd %s", &dir)
				if dir == ".." {
					path = path[:len(path)-1]
				} else {
					path = append(path, dir)
				}
			}
		} else if strings.HasPrefix(line, "dir") {
		} else {
			var size int
			var name string
			fmt.Sscanf(line, "%d %s", &size, &name)
			for i := 0; i <= len(path); i++ {
				fullDir := strings.Join(path[:len(path)-i], "/")
				sizes[fullDir] += size
			}
		}
	}
	sum := 0
	for _, size := range sizes {
		if size < 100000 {
			sum += size
		}
	}
	fmt.Println("Part 1:", sum)

	free := 70_000_000 - sizes["/"]
	needToDelete := 30_000_000 - free
	allSizes := make([]int, 0, len(sizes))
	for _, size := range sizes {
		allSizes = append(allSizes, size)
	}
	sort.Ints(allSizes)
	for i := 0; i < len(allSizes); i++ {
		if allSizes[i] >= needToDelete {
			fmt.Println("Part 2:", allSizes[i])
			break
		}
	}
}
