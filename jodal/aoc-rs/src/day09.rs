use itertools::Itertools;
use std::str::Lines;

pub fn solve_a(numbers: &Vec<u64>, preamble_size: usize) -> u64 {
    for (index, &number) in numbers.iter().skip(preamble_size).enumerate() {
        let preamble = &numbers[index..(index + preamble_size)];
        if !is_valid(number, preamble) {
            return number;
        }
    }
    panic!("No solution found");
}

pub fn solve_b(_numbers: &Vec<u64>) -> u64 {
    0
}

pub fn from_lines(lines: Lines) -> Vec<u64> {
    lines.map(|l| l.parse().unwrap()).collect()
}

fn is_valid(number: u64, preamble: &[u64]) -> bool {
    preamble
        .iter()
        .cartesian_product(preamble)
        .map(|(&a, &b)| a + b)
        .find(|&sum| sum == number)
        .is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_a() {
        let input = String::from(
            "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576",
        );
        let numbers = from_lines(input.lines());
        assert_eq!(solve_a(&numbers, 5), 127);
    }

    #[test]
    fn example_b() {
        let input = String::from("");
        let numbers = from_lines(input.lines());
        assert_eq!(solve_b(&numbers), 0);
    }
}
