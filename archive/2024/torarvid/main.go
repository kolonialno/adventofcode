package main

import (
	"fmt"
	"os"
	"reflect"
)

type Advent struct{}

func main() {
	if len(os.Args) < 2 {
		println("Please specify a day to run, like: aoc 01")
		return
	}
	v := os.Args[1]
	day := "Day" + v
	a := new(Advent)

	method := reflect.ValueOf(a).MethodByName(day)
	if method.IsValid() {
		method.Call(nil)
	} else {
		fmt.Printf("No impl for %s\n", day)
	}
}
