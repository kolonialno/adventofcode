package main

import (
    "fmt"
    "log"
    _ "os"
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
            password := match[4]

            letterPolicyRegex := regexp.MustCompile(letterPolicy)
            matches := letterPolicyRegex.FindAllString(password, -1)

            if len(matches) >= lower && len(matches) <= upper {
                valid = valid + 1
                fmt.Printf("%d-%d %s: %s (%d matches, valid)\n", lower, upper, letterPolicy, password, len(matches))
            }
        } else {
            fmt.Println("No match!")
        }

    }
    fmt.Println(valid)
}
