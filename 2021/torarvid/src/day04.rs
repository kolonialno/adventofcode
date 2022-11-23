use crate::util::file_by_lines;
use std::slice::Chunks;

#[derive(Debug, PartialEq, Copy, Clone)]
struct BingoEntry {
    number: u32,
    is_checked: bool,
}

#[derive(Debug, PartialEq, Clone)]
struct BingoBoard {
    entries: [[BingoEntry; 5]; 5],
}

impl From<&[String]> for BingoBoard {
    fn from(lines: &[String]) -> Self {
        let mut entries = [[BingoEntry {
            number: 0,
            is_checked: false,
        }; 5]; 5];
        for (i, line) in lines[1..].iter().enumerate() {
            for (j, number) in line
                .split_whitespace()
                .map(|n| n.parse().unwrap())
                .enumerate()
            {
                entries[i][j] = BingoEntry {
                    number,
                    is_checked: false,
                };
            }
        }
        BingoBoard { entries }
    }
}

impl BingoBoard {
    fn has_bingo(&self) -> bool {
        for i in 0..5 {
            // First check rows
            if self.entries[i].iter().all(|e| e.is_checked) {
                return true;
            }

            // Then check columns
            if self.entries[0][i].is_checked
                && self.entries[1][i].is_checked
                && self.entries[2][i].is_checked
                && self.entries[3][i].is_checked
                && self.entries[4][i].is_checked
            {
                return true;
            }
        }
        return false;
    }

    fn mark_number(&mut self, number: u32) {
        for i in 0..5 {
            for j in 0..5 {
                if self.entries[i][j].number == number {
                    self.entries[i][j].is_checked = true;
                }
            }
        }
    }

    fn calculate_score(&self, number: u32) -> u32 {
        let entries = self.entries.iter().flatten();
        entries
            .filter(|e| !e.is_checked)
            .map(|e| e.number)
            .sum::<u32>()
            * number
    }
}

pub fn run() {
    let lines = file_by_lines("day04.txt");

    let num_parts = lines[0].split(",");
    let numbers: Vec<_> = num_parts
        .map(|s| s.trim().parse::<u32>().unwrap())
        .collect();

    println!("Part1: {}", part1(lines[1..].chunks(6), &numbers));
    println!("Part2: {}", part2(lines[1..].chunks(6), &numbers));
}

fn part1(blocks: Chunks<String>, numbers: &Vec<u32>) -> u32 {
    let mut bingo_boards: Vec<_> = blocks.map(|block| BingoBoard::from(block)).collect();

    for number in numbers {
        for bingo_board in bingo_boards.iter_mut() {
            bingo_board.mark_number(*number);
            if bingo_board.has_bingo() {
                return bingo_board.calculate_score(*number);
            }
        }
    }
    panic!("No bingo found");
}

fn part2(blocks: Chunks<String>, numbers: &Vec<u32>) -> u32 {
    let mut bingo_boards: Vec<_> = blocks.map(|block| BingoBoard::from(block)).collect();

    for number in numbers {
        let mut bingo_boards_to_delete = Vec::new();
        let bingo_boards_length = bingo_boards.len();
        for (i, bingo_board) in bingo_boards.iter_mut().enumerate() {
            bingo_board.mark_number(*number);
            if bingo_board.has_bingo() {
                if bingo_boards_length == 1 {
                    return bingo_board.calculate_score(*number);
                }
                bingo_boards_to_delete.push(i);
            }
        }
        for i in bingo_boards_to_delete.iter().rev() {
            bingo_boards.remove(*i);
        }
    }
    panic!("no solution found");
}
