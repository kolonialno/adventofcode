package main

import (
    "fmt"
    "log"
    "regexp"
    "strconv"
    utils "torvald/libs"
)

func main() {
    lines, err := utils.ReadLinesStrings("input/2.txt")

    if err != nil {
        log.Fatalf("readLines: %s", err)
    }

    valid := 0

    for _, line := range lines {

        pattern := regexp.MustCompile(`(\d+)-(\d+) (.): (.*)$`)
        match := pattern.FindStringSubmatch(line)

        if match != nil {
            lower, _ := strconv.Atoi(match[1])
            upper, _ := strconv.Atoi(match[2])
            letterPolicy := match[3]
            password := []byte(match[4])

            letterPolicyRegex := regexp.MustCompile(letterPolicy)
            matches := letterPolicyRegex.FindAllIndex(password, -1)

            lowerFound := false
            upperFound := false

            for _, match := range matches {
                index := match[0] + 1
                if index == lower {
                    lowerFound = true
                }
                if index == upper {
                    upperFound = true
                }
            }

            if lowerFound != upperFound {
                fmt.Printf("%d-%d %s: %s (matches: %v)\n", lower, upper, letterPolicy, password, matches)
                valid = valid + 1
            }

        } else {
            fmt.Println("No match!")
        }

    }
    fmt.Println(valid)
}
