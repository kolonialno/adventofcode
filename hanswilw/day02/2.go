package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"
)

type Password struct {
	min        int
	max        int
	value      rune
	passphrase string
}

func importInput() []Password {
	file, err := os.Open("2.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	inputList := []Password{}

	for _, val := range s {
		a := strings.Split(val, " ")
		minmax := strings.Split(a[0], "-")
		min, _ := strconv.Atoi(minmax[0])
		max, _ := strconv.Atoi(minmax[1])
		p := Password{min: min, max: max, value: []rune(a[1])[0], passphrase: a[2]}
		inputList = append(inputList, p)
	}
	return inputList
}

func validatePasswords(passwords []Password) int {
	verified := 0

	for _, password := range passwords {
		countOfValue := 0
		for _, char := range password.passphrase {
			if char == password.value {
				countOfValue++
			}
		}
		if countOfValue >= password.min && countOfValue <= password.max {
			verified++
		}

	}

	return verified
}

func validatePasswords2(passwords []Password) int {
	verified := 0

	for _, password := range passwords {
		first := rune(password.passphrase[password.min-1])
		second := rune(password.passphrase[password.max-1])

		if first != second && (first == password.value || second == password.value) {
			verified++
		}
	}

	return verified
}

func main() {
	passwords := importInput()
	result := validatePasswords(passwords)
	fmt.Println(result)
	result2 := validatePasswords2(passwords)
	fmt.Println(result2)

}
