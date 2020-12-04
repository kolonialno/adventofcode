package main

import (
	"fmt"
	"log"
	"regexp"
	"strconv"
	"strings"
	utils "torvald/libs"
)

func validPassport(passport map[string]string) bool {

	requiredFields := [...]string{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

	for _, requiredField := range requiredFields {
		if _, ok := passport[requiredField]; !ok {
			return false
		}

		value := passport[requiredField]

		switch requiredField {
		case "byr":
			byr, _ := strconv.Atoi(value)
			if !(byr >= 1920 && byr <= 2002) {
				return false
			}
		case "iyr":
			iyr, _ := strconv.Atoi(value)
			if !(iyr >= 2010 && iyr <= 2020) {
				return false
			}
		case "eyr":
			eyr, _ := strconv.Atoi(value)
			if !(eyr >= 2020 && eyr <= 2030) {
				return false
			}
		case "hgt":
			pattern := regexp.MustCompile(`([0-9]+)(in|cm)`)
			match := pattern.FindStringSubmatch(value)
			if match == nil {
				return false
			}
			hgt, _ := strconv.Atoi(match[1])
			metric := match[2]
			switch metric {
			case "cm":
				if !(hgt >= 150 && hgt <= 193) {
					return false
				}
			case "in":
				if !(hgt >= 59 && hgt <= 76) {
					return false
				}
			}
		case "hcl":
			pattern := regexp.MustCompile(`^(#[a-f0-9]{6})$`)
			match := pattern.FindStringSubmatch(value)
			if match == nil {
				return false
			}
		case "ecl":
			pattern := regexp.MustCompile(`^(amb|blu|brn|gry|grn|hzl|oth)$`)
			match := pattern.FindStringSubmatch(value)
			if match == nil {
				return false
			}
		case "pid":
			pattern := regexp.MustCompile(`^([0-9]{9}$)`)
			match := pattern.FindStringSubmatch(value)
			if match == nil {
				return false
			}

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
