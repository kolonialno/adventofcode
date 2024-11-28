use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

fn read<R: Read>(io: R) -> Result<(Vec<Vec<Vec<u8>>>, Vec<u8>), Error> {
    let mut br = BufReader::new(io);

    // First line contains number oarder
    let mut first_line = String::new();
    br.read_line(&mut first_line).unwrap();
    let first_line = first_line
        .trim()
        .split(',')
        .map(|i| i.parse().unwrap())
        .collect();

    // Read the rest of the lines
    let mut board_line = 0;
    let mut v = vec![];
    let mut vv = vec![];
    for line in br.lines() {
        if board_line > 0 {
            vv.push(
                line?
                    .trim()
                    .split(' ')
                    .filter(|&i| i.len() != 0)
                    .map(|i| i.parse().unwrap())
                    .collect(),
            );
        }
        board_line += 1;

        if board_line == 6 {
            // Read next board
            board_line = 0;
            v.push(vv);
            vv = vec![];
        }
    }
    Ok((v, first_line))
}

fn cross_number_off(boards: &Vec<Vec<Vec<u8>>>, hits: &mut Vec<Vec<Vec<bool>>>, num: u8) {
    for (i, board) in boards.iter().enumerate() {
        for (ii, line) in board.iter().enumerate() {
            for (iii, check_num) in line.iter().enumerate() {
                if check_num == &num {
                    hits[i][ii][iii] = true;
                    // Coulf break back up to board level from here.. but shrug
                }
            }
        }
    }
}

fn check_bingo(hits: &Vec<Vec<Vec<bool>>>, boards_done: &Vec<usize>) -> Option<usize> {
    for (i, board) in hits.iter().enumerate() {
        // Check horizontal line
        for line in board.iter() {
            if line.iter().all(|&x| x) & !boards_done.contains(&i) {
                return Some(i);
            }
        }
        // Check vertical line
        for ii in 0..5 {
            if (0..5).map(|iii| hits[i][iii][ii]).all(|x| x) & !boards_done.contains(&i) {
                return Some(i);
            }
        }
    }
    None
}

fn calculate_board_score(board: &Vec<Vec<u8>>, hits: &Vec<Vec<bool>>, num: u8) {
    let mut board_sum = 0;
    for (i, line) in hits.iter().enumerate() {
        for (ii, &hit) in line.iter().enumerate() {
            if !hit {
                board_sum += board[i][ii] as u64
            }
        }
    }
    println!("The board that won {}", board_sum as u64 * num as u64);
}

pub fn day04() {
    let (boards, all_numbers) = read(File::open("inputs/day04.txt").unwrap()).unwrap();
    let mut hits = vec![vec![vec![false; 5]; 5]; boards.len()];

    let mut boards_done = vec![];

    for num in &all_numbers {
        cross_number_off(&boards, &mut hits, *num);

        let mut all_boards = false;
        while !all_boards {
            let won_board = check_bingo(&hits, &boards_done);
            if let Some(won_board_num) = won_board {
                if !boards_done.contains(&won_board_num) {
                    boards_done.push(won_board_num)
                }
                if boards_done.len() == boards.len() {
                    let won_board_num = boards_done.pop().unwrap();
                    calculate_board_score(&boards[won_board_num], &hits[won_board_num], *num);
                    return;
                }
            } else {
                all_boards = true;
            }
        }
    }
}
