use regex::Regex;
use std::fs::File;
use std::io::{BufRead, BufReader};

struct Move {
    num: usize,
    from: usize,
    to: usize,
}

type S = [Vec<char>; 9];
type M = Vec<Move>;

fn read_input(filename: &str) -> (S, M) {
    let file = File::open(filename).expect("No file found!");
    let reader = BufReader::new(file);

    let mut moves: M = vec![];
    let mut stacks: S = [(); 9].map(|_| Vec::with_capacity(29));

    let re = Regex::new(r"move (?P<num>\d*) from (?P<from>\d*) to (?P<to>\d*)").unwrap();

    for line in reader.lines().map(|l| l.expect("Could not parse line!")) {
        if re.is_match(&line) {
            let caps = re.captures(&line).unwrap();
            moves.push(Move {
                num: caps["num"].parse::<usize>().unwrap(),
                from: caps["from"].parse::<usize>().unwrap() - 1,
                to: caps["to"].parse::<usize>().unwrap() - 1,
            });

        // Manually parse input to make this part less annoying
        } else {
            let arr: Vec<char> = line.chars().rev().collect();
            let index: u32 = arr[0].to_digit(10).unwrap() - 1;
            stacks[index as usize] = arr[1..].to_vec();
        }
    }
    (stacks, moves)
}

fn get_string(stacks: S) -> String {
    stacks
        .into_iter()
        .filter(|v| v.len() > 0)
        .map(|v| v[v.len() - 1])
        .collect()
}
fn problem_1(mut stacks: S, moves: M) -> String {
    for m in moves {
        for _ in 0..m.num {
            let popped = stacks[m.from].pop().unwrap();
            stacks[m.to].push(popped);
        }
    }
    get_string(stacks)
}

fn problem_2(mut stacks: S, moves: M) -> String {
    for m in moves {
        let index = stacks[m.from].len() - m.num;
        let popped = stacks[m.from].split_off(index);
        stacks[m.to].extend(popped);
    }
    get_string(stacks)
}

fn main() {
    println!("Hello, day 5!");

    let (test_stacks, test_moves) = read_input("test_input.txt");
    assert_eq!(problem_1(test_stacks, test_moves), "CMZ");

    let (stacks, moves) = read_input("input.txt");
    println!("Solution to problem 1:{}", problem_1(stacks, moves));

    let (test_stacks, test_moves) = read_input("test_input.txt");
    assert_eq!(problem_2(test_stacks, test_moves), "MCD");

    let (stacks, moves) = read_input("input.txt");
    println!("Solution to problem 2:{}", problem_2(stacks, moves));
}
