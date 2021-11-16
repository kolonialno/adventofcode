package main

import (
	"fmt"
	"log"
	_ "regexp"
	_ "strconv"
	"strings"
	utils "torvald/libs"
)

func validPassport(passport map[string]string) bool {

	requiredFields := [...]string{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

	for _, requiredField := range requiredFields {
		if _, ok := passport[requiredField]; !ok {
			return false
		}
	}
	return true
}

func main() {
	lines, err := utils.ReadLinesStrings("input/4.txt")

	if err != nil {
		log.Fatalf("readLines: %s", err)
	}

	valid := 0

	passport := make(map[string]string)
	passports := make([]map[string]string, 0)

	for _, line := range lines {

		if line == "" {
			passports = append(passports, passport)
			passport = make(map[string]string)
			continue
		}

		for _, keypair := range strings.Split(line, " ") {
			split := strings.Split(keypair, ":")
			passportKey := split[0]
			passportValue := split[1]
			passport[passportKey] = passportValue
		}
	}

	for _, passport := range passports {
		if validPassport(passport) {
			valid++
		}
	}

	fmt.Println(valid)
}
