use std::fs::{self};

#[derive(Copy, Debug, Clone)]
struct MoveOperation {
    from: usize,
    to: usize,
    amount: usize,
}

impl MoveOperation {
    fn parse(input: &str) -> Self {
        let parts = input.split_whitespace().collect::<Vec<&str>>();
        Self {
            from: parts[3].parse::<usize>().unwrap() - 1,
            to: parts[5].parse::<usize>().unwrap() - 1,
            amount: parts[1].parse::<usize>().unwrap(),
        }
    }

    fn apply_9000(&self, stacks: &mut [Vec<char>]) {
        for _ in 0..self.amount {
            let x = stacks[self.from].pop().unwrap();
            stacks[self.to].push(x);
        }
    }

    fn apply_9001(&self, stacks: &mut [Vec<char>]) {
        let from_len = stacks[self.from].len();
        let removed_elements = stacks[self.from]
            .drain(from_len - self.amount..)
            .collect::<Vec<char>>();
        stacks[self.to].extend(removed_elements.iter());
    }
}

fn main() {
    const INPUT_FILE: &str = "05/input.txt";
    let input = fs::read_to_string(INPUT_FILE).unwrap();
    let mut parts = input.split("\n\n");
    let mut stacks: Vec<Vec<char>> = Vec::new();
    let mut stack_iter = parts.next().unwrap().lines().rev();
    let first_line = stack_iter.next();
    for _ in first_line.unwrap().split_whitespace() {
        stacks.push(Vec::default());
    }
    for line in stack_iter {
        for (i, c) in line.chars().skip(1).step_by(4).enumerate() {
            if !c.is_whitespace() {
                stacks[i].push(c);
            }
        }
    }

    let mut stacks_9000 = stacks.clone();
    let mut stacks_9001 = stacks;
    let operations = parts.next().unwrap().lines().map(MoveOperation::parse);
    for op in operations {
        op.apply_9000(&mut stacks_9000);
        op.apply_9001(&mut stacks_9001);
    }
    println!(
        "Top crates 9000: {}",
        stacks_9000.iter().map(|x| x.last().unwrap()).fold(
            String::new(),
            |mut top_crates, elem| {
                top_crates.push(*elem);
                top_crates
            }
        )
    );
    println!(
        "Top crates 9001: {}",
        stacks_9001.iter().map(|x| x.last().unwrap()).fold(
            String::new(),
            |mut top_crates, elem| {
                top_crates.push(*elem);
                top_crates
            }
        )
    );
}
