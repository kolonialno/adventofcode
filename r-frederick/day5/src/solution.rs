use regex::Regex;
use std::collections::VecDeque;

fn parse_instruction(ins: &str) -> (usize, usize, usize) {
    let re = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    if let Some(caps) = re.captures(ins) {
        return (
            caps[1].parse::<usize>().unwrap(),
            caps[2].parse::<usize>().unwrap(),
            caps[3].parse::<usize>().unwrap(),
        );
    }

    (0, 0, 0)
}

pub fn solve_part1(state: &mut Vec<VecDeque<&str>>, instructions: &Vec<&str>) -> String {
    for inst in instructions.iter() {
        let (amount, from, to) = parse_instruction(inst);
        let source = &mut state[from - 1];

        let drained: Vec<&str> = source.drain(..amount).collect();

        let sink = &mut state[to - 1];

        for item in drained.iter() {
            sink.push_front(*item);
        }
    }

    state.iter().fold("".to_string(), |tops, stack| {
        tops + &stack.front().unwrap_or(&" ").to_string()
    })
}

pub fn solve_part2(state: &mut Vec<VecDeque<&str>>, instructions: &Vec<&str>) -> String {
    for inst in instructions.iter() {
        let (amount, from, to) = parse_instruction(inst);
        let source = &mut state[from - 1];

        let drained: Vec<&str> = source.drain(..amount).collect();

        let sink = &mut state[to - 1];

        for item in drained.iter().rev() {
            sink.push_front(*item);
        }
    }

    state.iter().fold("".to_string(), |tops, stack| {
        tops + &stack.front().unwrap_or(&" ").to_string()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_part1_returns_correct_answer() {
        let mut start_state = vec![
            VecDeque::from(["N", "Z"]),
            VecDeque::from(["D", "C", "M"]),
            VecDeque::from(["P"]),
        ];

        let instructions = vec![
            "move 1 from 2 to 1",
            "move 3 from 1 to 3",
            "move 2 from 2 to 1",
            "move 1 from 1 to 2",
        ];

        let expected = "CMZ";

        assert_eq!(solve_part1(&mut start_state, &instructions), expected);
    }

    #[test]
    fn solve_part2_returns_correct_answer() {
        let mut start_state = vec![
            VecDeque::from(["N", "Z"]),
            VecDeque::from(["D", "C", "M"]),
            VecDeque::from(["P"]),
        ];

        let instructions = vec![
            "move 1 from 2 to 1",
            "move 3 from 1 to 3",
            "move 2 from 2 to 1",
            "move 1 from 1 to 2",
        ];

        let expected = "MCD";

        assert_eq!(solve_part2(&mut start_state, &instructions), expected);
    }

    #[test]
    fn parse_instruction_works() {
        assert_eq!(parse_instruction("move 1 from 2 to 1"), (1, 2, 1))
    }
}
