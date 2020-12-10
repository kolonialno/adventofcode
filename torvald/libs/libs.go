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

func ReadLinesNumbersMap(path string) (map[int]bool, int, int, error) {

    numbersSlice, err := ReadLinesNumbers(path)

	if err != nil {
		return nil, 0, 0, err
	}

	numbers := make(map[int]bool)

    min, max := 0, 0

	for _, number := range numbersSlice {
        numbers[number] = true
        if number > max { max = number }
        if number < min { min = number }
    }

    return numbers, min, max, err
}

func ReadRuneMatrix(path string) ([][]rune, error) {

	lines, err := ReadLinesStrings(path)

	if err != nil {
		return nil, err
	}

	array := make([][]rune, 0)

	for _, line := range lines {
		row := make([]rune, 0)
		for _, char := range []rune(line) {
			row = append(row, char)
		}
		array = append(array, row)
	}

	return array, nil
}
