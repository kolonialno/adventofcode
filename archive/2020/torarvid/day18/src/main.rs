use std::fs;

fn main() {
    let content = fs::read_to_string("input.txt").expect("Failed to read input.txt");
    println!("Part1: {}", solve(&content));
    // println!("Part2: {}", solve_part2(&content));
}

fn solve(input: &str) -> usize {
    let mut stack = Vec::new();
    let mut op: Option<char> = None;
    let foo = |digit: char| match op {
        Some(o) if o == '*' => {
            stack.push(stack.pop().unwrap() * digit.to_digit(10).unwrap());
            op = None;
        }
        Some(o) if o == '+' => {
            stack.push(stack.pop().unwrap() + digit.to_digit(10).unwrap());
            op = None;
        }
        _ => stack.push(digit.to_digit(10).unwrap()),
    };
    for (index, ch) in input.chars().enumerate() {
        match ch {
            '(' => stack.push(0),
            ')' => stack.pop().unwrap(),
            '*' => op = Some('*'),
            '+' => op = Some('+'),
            digit => foo(digit),
        }
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_perm() {
        assert_eq!(solve("1 + 2 * 3 + 4 * 5 + 6"), 71);
    }
}
