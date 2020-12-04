package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type Field struct {
	key   string
	value string
}

func importInput() [][]Field {
	file, err := os.Open("4.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")

	inputList := [][]Field{}

	currentPassport := []Field{}
	for _, i := range s {
		if len(i) == 0 {
			if len(currentPassport) > 0 {
				inputList = append(inputList, currentPassport)
				currentPassport = []Field{}
			}
			continue
		}
		line := strings.Split(i, " ")
		for _, j := range line {
			pair := strings.Split(j, ":")
			currentPassport = append(currentPassport, Field{key: pair[0], value: pair[1]})
		}
	}
	if len(currentPassport) > 0 {
		inputList = append(inputList, currentPassport)
	}
	return inputList
}

func validatePassports(passportList [][]Field) int {
	validPassport := map[string]bool{
		"byr": true,
		"iyr": true,
		"eyr": true,
		"hgt": true,
		"hcl": true,
		"ecl": true,
		"pid": true,
		"cid": false,
	}
	validPassports := 0

	for _, passportValues := range passportList {
		numberOfRequiredFields := 0
		for _, field := range passportValues {
			if required, ok := validPassport[field.key]; ok {
				if ok && required {
					numberOfRequiredFields++
				}
			}
		}

		if numberOfRequiredFields >= 7 {
			validPassports++
		}

	}
	return validPassports

}

func validateByr(value string) bool {
	intValue, err := strconv.Atoi(value)
	if err != nil {
		return false
	}
	return 1920 <= intValue && intValue <= 2002
}

func validateIyr(value string) bool {
	intValue, err := strconv.Atoi(value)
	if err != nil {
		return false
	}
	return 2010 <= intValue && intValue <= 2020
}
func validateEyr(value string) bool {
	intValue, err := strconv.Atoi(value)
	if err != nil {
		return false
	}
	return 2020 <= intValue && intValue <= 2030
}
func validateHgt(value string) bool {
	r := regexp.MustCompile(`([0-9]{2,3})(cm|in)`)
	res := r.FindStringSubmatch(value)
	if len(res) == 0 {
		return false
	}
	intValue, _ := strconv.Atoi(res[1])
	if res[2] == "cm" {
		return intValue >= 150 && intValue <= 193
	}
	return intValue >= 59 && intValue <= 76
}
func validateHcl(value string) bool {
	matched, err := regexp.MatchString(`#([a-z]|[0-9]){6}`, value)
	if err != nil {
		return false
	}
	return matched
}
func validateEcl(value string) bool {
	valid := map[string]bool{
		"amb": true,
		"blu": true,
		"brn": true,
		"gry": true,
		"grn": true,
		"hzl": true,
		"oth": true,
	}
	return valid[value] == true
}
func validatePid(value string) bool {
	_, err := strconv.Atoi(value)
	if err != nil {
		return false
	}
	return len(value) == 9
}
func validateCid(value string) bool {
	return false
}

func validatePassports2(passportList [][]Field) int {
	validPassport := map[string]func(string) bool{
		"byr": validateByr,
		"iyr": validateIyr,
		"eyr": validateEyr,
		"hgt": validateHgt,
		"hcl": validateHcl,
		"ecl": validateEcl,
		"pid": validatePid,
		"cid": validateCid,
	}
	validPassports := 0

	for _, passportValues := range passportList {
		numberOfRequiredFields := 0
		for _, field := range passportValues {
			if validateMethod, ok := validPassport[field.key]; ok {
				if ok && validateMethod(field.value) {
					numberOfRequiredFields++
				}
			}
		}

		if numberOfRequiredFields >= 7 {
			validPassports++
		}

	}
	return validPassports

}

func main() {
	passports := importInput()
	result := validatePassports(passports)
	fmt.Println(result)
	result2 := validatePassports2(passports)
	fmt.Println(result2)
}
