package main

import (
    "fmt"
    "log"
    "os"
    utils "torvald/libs"
)

func main() {
    numbers, err := utils.ReadLinesNumbers("input/1.txt")

    if err != nil {
        log.Fatalf("readLines: %s", err)
    }

    for _, x := range numbers {
        for _, y := range numbers {
            for _, z := range numbers {
                if x+y+z == 2020 {
                    fmt.Println(x * y * z)
                    os.Exit(0)
                }
            }
        }
    }
}
