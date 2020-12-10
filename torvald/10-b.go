package main

import (
	"fmt"
	utils "torvald/libs"
)

func main() {
    numbers, _, max, _ := utils.ReadLinesNumbersMap("input/10.txt")
    seen := make(map[int]int)

    var foo func(from int) int
    foo = func(from int) int {
        e := 0

        // lets not calc what we already know
        if _, exists := seen[from]; exists { return seen[from] }

        if from == max { return 1 }

        for next := from+1; next <= from+3; next++ {
            if _, exists := numbers[next]; exists { e += foo(next) }
        }

        seen[from] = e

        return e
    }

    fmt.Println(foo(0))
}
