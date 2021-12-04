#![feature(vec_retain_mut)]
use std::collections::HashMap;

const SIZE: usize = 5;

fn part1(mut boards: Vec<Board>, commands: &str) -> i32 {
    for c in commands.split(",") {
        if let Some(board) = boards.iter_mut().find_map(|board| {
            if board.add_and_check(c) {
                Some(board)
            } else {
                None
            }
        }) {
            return c.parse::<i32>().unwrap() * board.sum_unselected_squares();
        }
    }
    0
}

fn part2(mut boards: Vec<Board>, commands: &str) -> i32 {
    for c in commands.split(",") {
        if boards.len() > 1 {
            boards.retain_mut(|board| !board.add_and_check(c));
            continue;
        }
        if boards[0].add_and_check(c) {
            return c.parse::<i32>().unwrap() * boards[0].sum_unselected_squares();
        }
    }
    0
}
fn main() {
    let mut input = include_str!("../input.txt")
        .split("\n")
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>();

    let commands = input.remove(0);
    let boards = input
        .chunks(SIZE)
        .map(|c| {
            c.into_iter()
                .map(|s| s.split_whitespace().collect::<Vec<_>>())
                .collect::<Vec<_>>()
        })
        .map(Board::from)
        .collect::<Vec<_>>();

    println!("{}", part1(boards.clone(), commands));
    println!("{}", part2(boards.clone(), commands));
}

#[derive(Default, Debug, Clone)]
struct Board(HashMap<String, [usize; 3]>);

impl Board {
    fn sum_unselected_squares(&self) -> i32 {
        self.0
            .iter()
            .filter(|(_, [_, _, sel])| *sel == 0)
            .map(|(v, _)| v.parse::<i32>().unwrap())
            .sum()
    }

    fn get_selected_squares(&self) -> Vec<[&usize; 2]> {
        self.0
            .iter()
            .filter(|(_, [_, _, sel])| *sel == 1)
            .map(|(_, [row, col, _])| [row, col])
            .collect::<Vec<_>>()
    }

    fn add_and_check(&mut self, ball: &str) -> bool {
        if let Some([_, _, sel]) = self.0.get_mut(&String::from(ball)) {
            *sel = 1;
            return self.check_win();
        }
        false
    }

    fn check_win(&self) -> bool {
        let selected = self.get_selected_squares();
        for i in 0..SIZE {
            if selected
                .iter()
                .filter(|[col, _]| **col == i as usize)
                .count()
                == SIZE
            {
                return true;
            }
            if selected
                .iter()
                .filter(|[_, row]| **row == i as usize)
                .count()
                == SIZE
            {
                return true;
            }
        }
        false
    }
}

impl From<Vec<Vec<&str>>> for Board {
    fn from(b: Vec<Vec<&str>>) -> Self {
        let mut board = Board::default();
        for row in 0..b.len() {
            for col in 0..b[0].len() {
                board.0.insert(b[row][col].to_string(), [row, col, 0]);
            }
        }
        board
    }
}
