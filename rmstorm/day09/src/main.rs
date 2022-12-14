use itertools::Itertools;
use regex::Regex;

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Up(i32),
    Right(i32),
    Down(i32),
    Left(i32),
}
fn update_knot_position(leading_knot: (i32, i32), follow_knot: &mut (i32, i32)) {
    *follow_knot = match (
        leading_knot.0 - follow_knot.0,
        leading_knot.1 - follow_knot.1,
    ) {
        (-1..=1, -1..=1) => (follow_knot.0, follow_knot.1),
        (-1..=1, -2) => (leading_knot.0, leading_knot.1 + 1),
        (-1..=1, 2) => (leading_knot.0, leading_knot.1 - 1),
        (-2, -1..=1) => (leading_knot.0 + 1, leading_knot.1),
        (2, -1..=1) => (leading_knot.0 - 1, leading_knot.1),
        (a, b) => (leading_knot.0 - a / 2, leading_knot.1 - b / 2),
    };
}
fn update_tail_pos(
    head_pos: (i32, i32),
    tail_pos: &mut [(i32, i32); 9],
    visited: &mut Vec<(i32, i32)>,
) {
    update_knot_position(head_pos, &mut tail_pos[0]);
    for i in 0..8 {
        update_knot_position(tail_pos[i], &mut tail_pos[i + 1]);
    }
    visited.push(tail_pos[8].clone())
}

fn main() {
    let input = include_str!("input.txt");
    let re = Regex::new(r"^(\w+) (\d+)$").unwrap();
    let commands: Vec<Instruction> = input
        .lines()
        .map(|line| {
            let cap = re.captures(line).unwrap();
            match &cap[1] {
                "U" => return Instruction::Up(i32::from_str_radix(&cap[2], 10).unwrap()),
                "R" => return Instruction::Right(i32::from_str_radix(&cap[2], 10).unwrap()),
                "D" => return Instruction::Down(i32::from_str_radix(&cap[2], 10).unwrap()),
                "L" => return Instruction::Left(i32::from_str_radix(&cap[2], 10).unwrap()),
                _ => panic!("Oh noO"),
            }
        })
        .collect();
    let mut visited = vec![(0, 0)];
    let mut tail_pos = [(0, 0); 9];
    let mut head_pos = (0, 0);
    for command in commands.iter() {
        match command {
            Instruction::Up(n) => {
                for _ in 0..*n {
                    head_pos = (head_pos.0 + 1, head_pos.1);
                    update_tail_pos(head_pos, &mut tail_pos, &mut visited);
                }
            }
            Instruction::Right(n) => {
                for _ in 0..*n {
                    head_pos = (head_pos.0, head_pos.1 + 1);
                    update_tail_pos(head_pos, &mut tail_pos, &mut visited);
                }
            }
            Instruction::Down(n) => {
                for _ in 0..*n {
                    head_pos = (head_pos.0 - 1, head_pos.1);
                    update_tail_pos(head_pos, &mut tail_pos, &mut visited);
                }
            }
            Instruction::Left(n) => {
                for _ in 0..*n {
                    head_pos = (head_pos.0, head_pos.1 - 1);
                    update_tail_pos(head_pos, &mut tail_pos, &mut visited);
                }
            }
        }
    }
    dbg!(visited.into_iter().unique().count());
}
