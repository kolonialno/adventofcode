type Crate = char;
type Stack = Vec<Crate>;
type Stacks = Vec<Stack>;

fn parse_row(s: &str) -> Vec<Crate> {
    s.chars().skip(1).step_by(4).collect()
}

fn parse_stacks(s: &str) -> Stacks {
    let lines: Vec<&str> = s.lines().collect();
    // let last = lines.last().expect("Expected at least one input line");
    let rows: Vec<Vec<Crate>> = lines.iter().take(lines.len()-1).map(|line| parse_row(line)).collect();

    let mut stacks: Stacks = vec![];
    for i in 0..rows[0].len() {
        let stack: Stack = rows.iter().map(|row| row[i]).filter(|item| !item.is_whitespace()).collect();
        stacks.push(stack);
    }

    stacks
}

#[derive(Debug)]
struct Operation {
    num_crates: usize,
    from: usize,
    to: usize,
}

fn parse_operation(s: &str) -> Operation {
    let numbers: Vec<usize> = s.split(" ").filter(|s| s.chars().all(|c| c.is_digit(10))).map(|s| s.parse::<usize>().unwrap()).collect();
    assert_eq!(numbers.len(), 3, "Expected to find three numbers on the line");
    Operation { num_crates: numbers[0], from: numbers[1], to: numbers[2] }
}

fn execute_in_sequence(stacks: &mut Stacks, operation: &Operation) {
    for _ in 0..operation.num_crates {
        let popped = stacks[operation.from - 1].remove(0);
        stacks[operation.to - 1].insert(0, popped);
    }
}

fn execute_in_batch(stacks: &mut Stacks, operation: &Operation) {
    for i in 0..operation.num_crates {
        let popped = stacks[operation.from - 1].remove(0);
        stacks[operation.to - 1].insert(i, popped);
    }
}


fn solve_part1(input: &str) -> String {
    let (stacks, operations) = input.split_once("\n\n").expect("Expected to find and empty line");
    let mut stacks = parse_stacks(stacks);
    let operations: Vec<Operation> = operations.lines().map(parse_operation).collect();

    for operation in operations {
        execute_in_sequence(&mut stacks, &operation);
    }

    stacks.iter().filter(|stack| !stack.is_empty()).map(|stack| stack[0]).collect::<String>()
}

fn solve_part2(input: &str) -> String {
    let (stacks, operations) = input.split_once("\n\n").expect("Expected to find and empty line");
    let mut stacks = parse_stacks(stacks);
    let operations: Vec<Operation> = operations.lines().map(parse_operation).collect();

    for operation in operations {
        execute_in_batch(&mut stacks, &operation);
    }

    stacks.iter().filter(|stack| !stack.is_empty()).map(|stack| stack[0]).collect::<String>()
}

fn main() {
    let input = include_str!("./input.txt");
    let result = solve_part1(input);
    println!("Part 1: {result:?}");
    let result = solve_part2(input);
    println!("Part 2: {result:?}");
}


#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_solve_par1() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part1(input), "CMZ");
    }

    #[test]
    fn test_solve_part2() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part2(input), "MCD");
    }
}
