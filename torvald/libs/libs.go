package libs

import (
    "bufio"
    "os"
    "strconv"
)

func readLines(path string) ([]string, error) {
    file, err := os.Open(path)
    if err != nil {
        return nil, err
    }
    defer file.Close()

    var lines []string
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        lines = append(lines, scanner.Text())
    }
    return lines, scanner.Err()
}

func ReadLinesStrings(path string) ([]string, error) {
    return readLines(path)
}

func ReadLinesNumbers(path string) ([]int, error) {
    lines, err := readLines(path)

    if err != nil {
        return nil, err
    }

    var numbers = []int{}

    for _, i := range lines {
        j, err := strconv.Atoi(i)
        if err != nil {
            return nil, err
        }
        numbers = append(numbers, j)
    }

    return numbers, nil
}
