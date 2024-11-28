use std::ops::RangeInclusive;

type Assignment = RangeInclusive<i32>;

/// Given two assignments, check if one is fully contained by the other
fn is_contained_by(assignments: &(Assignment, Assignment)) -> bool {
    let (first, second) = assignments;
    first.contains(second.start()) && first.contains(second.end())
        || second.contains(first.start()) && second.contains(first.end())
}

/// Given two assignments, check if there's any overlap
fn has_overlap(assignments: &(Assignment, Assignment)) -> bool {
    let (first, second) = assignments;
    first.contains(second.start())
        || first.contains(second.end())
        || second.contains(first.start())
        || second.contains(first.end())
}

/// Convert an input string, e.g. "2-3", into an assignment range
fn parse_assignment(string: &str) -> Assignment {
    let numbers: Vec<i32> = string
        .splitn(2, "-")
        .map(|n| n.parse::<i32>().expect("Invalid input, expected a number"))
        .collect();
    assert_eq!(numbers.len(), 2, "Expected two numbers separated by a '-'");
    Assignment::new(numbers[0], numbers[1])
}

// Parse a line, e.g. "2-3,4-5", into a pair of assignment ranges
fn parse_assignment_pair(line: &str) -> (Assignment, Assignment) {
    let parts: Vec<Assignment> = line.split(",").map(parse_assignment).collect();
    assert_eq!(parts.len(), 2, "Expected exactly one comma");
    (parts[0].clone(), parts[1].clone())
}

fn solve_part1(input: &str) -> usize {
    input
        .lines()
        .map(parse_assignment_pair)
        .filter(is_contained_by)
        .count()
}

fn solve_part2(input: &str) -> usize {
    input
        .lines()
        .map(parse_assignment_pair)
        .filter(has_overlap)
        .count()
}

fn main() {
    let input = include_str!("./input.txt");
    let sum = solve_part1(input);
    println!("Part 1: {sum}");
    let sum = solve_part2(input);
    println!("Part 2: {sum}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part1(input), 2);
    }

    #[test]
    fn test_part2() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part2(input), 4);
    }
}
