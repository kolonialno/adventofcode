package main

import (
	"fmt"
	utils "torvald/libs"
)

func main() {
    numbers, _, max, _ := utils.ReadLinesNumbersMap("input/10_test2.txt")

    differences := make(map[int]int)

    for i := 0; i < max; i++ {
        for next := 1; next <= 3; next++ {
            if _, exists := numbers[i+next]; exists {
                differences[next]++
                i += next - 1 
                break
            }
        }
    }

    differences[3]++
    fmt.Println(differences[1] * differences[3])
}
