package main

import (
    "fmt"
    "sort"
    "strconv"
    "strings"
    utils "torvald/libs"
)

func main() {
    lines, _ := utils.ReadLinesStrings("input/5.txt")

    seatIDs := make([]int, 0)

    for _, line := range lines {
        line = strings.Replace(line, "F", "0", -1)
        line = strings.Replace(line, "B", "1", -1)
        line = strings.Replace(line, "L", "0", -1)
        line = strings.Replace(line, "R", "1", -1)

        row, _ := strconv.ParseInt(line[0:7], 2, 64)
        col, _ := strconv.ParseInt(line[7:len(line)], 2, 64)

        seatID := (row * 8) + col
        seatIDs = append(seatIDs, int(seatID))
    }

    sort.Ints(seatIDs)

    for i, seatID := range seatIDs {
        distanceToNeighbor := seatIDs[i+1] - seatID
        if distanceToNeighbor != 1 {
            fmt.Println(seatID + 1)
            break
        }
    }
}
