package main

import (
	"bufio"
	"fmt"
	"os"
)

type answer struct {
	partOne int
	partTwo int
}

var rock = 1
var paper = 2
var scissors = 3

var lose = 0
var draw = 3
var win = 6

var winHand = map[int]int{
	rock:     scissors,
	paper:    rock,
	scissors: paper,
}

var loseHand = map[int]int{
	scissors: rock,
	rock:     paper,
	paper:    scissors,
}

func scoreRound(player int, opponent int) int {

	if player == opponent {
		return draw + player
	}

	if winHand[player] == opponent {
		return win + player
	}

	return lose + player // :-(

}

func scoreRoundOne(line string) int {

	inputMap := map[string]int{
		"A": rock,
		"B": paper,
		"C": scissors,
		"X": rock,
		"Y": paper,
		"Z": scissors,
	}

	opponent := inputMap[string(line[0])]
	player := inputMap[string(line[2])]

	return scoreRound(player, opponent)

}

func scoreRoundTwo(line string) int {

	inputMap := map[string]int{
		"A": rock,
		"B": paper,
		"C": scissors,
		"X": lose,
		"Y": draw,
		"Z": win,
	}

	opponent := inputMap[string(line[0])]
	strategy := inputMap[string(line[2])]
	if strategy == win {
		return scoreRound(loseHand[opponent], opponent)
	}

	if strategy == lose {
		return scoreRound(winHand[opponent], opponent)
	}

	if strategy == draw {
		return scoreRound(opponent, opponent)
	}

	return -1

}

func solve(filePath string) answer {

	input, _ := os.Open(filePath)
	defer input.Close()

	roundOneSum := 0
	roundTwoSum := 0
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		roundOneSum += scoreRoundOne(scanner.Text())
		roundTwoSum += scoreRoundTwo(scanner.Text())
	}

	return answer{partOne: roundOneSum, partTwo: roundTwoSum}

}

func main() {

	filePath := "day02.txt"
	fmt.Println(solve(filePath))

}
