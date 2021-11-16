package main

import (
	"fmt"
	"strconv"
	"strings"
	utils "torvald/libs"
)


func main() {
	lines, _ := utils.ReadLinesStrings("input/5.txt")

    highestSeatID := int64(0)

	for _, line := range lines {
        line = strings.Replace(line, "F", "0", -1)
        line = strings.Replace(line, "B", "1", -1)
        line = strings.Replace(line, "L", "0", -1)
        line = strings.Replace(line, "R", "1", -1)

        row, _ := strconv.ParseInt(line[0:7], 2, 64)
        col, _ := strconv.ParseInt(line[7:len(line)], 2, 64)

        seatID := (row * 8) + col

        if seatID > highestSeatID {
            highestSeatID = seatID
        }

        
    }

	fmt.Println(highestSeatID)
}
