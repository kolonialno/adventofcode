use std::format;
use std::fs;

fn main() {
    let filename = "input.txt";
    let mut numbers: Vec<i64> = fs::read_to_string(filename)
        .expect(&format!("Failed to read file {}", filename))
        .lines()
        .map(|line| line.parse::<i64>().unwrap())
        .collect();
    numbers.sort_unstable();
    numbers.insert(0, 0);
    numbers.push(numbers.last().unwrap() + 3);
    println!("Part1: {}", solve_part1(&numbers));
    println!("Part2: {}", solve_part2(&numbers));
}

fn solve_part1(numbers: &Vec<i64>) -> i64 {
    let mut delta_ones: i64 = 0;
    let mut delta_threes: i64 = 0;
    for i in 1..numbers.len() {
        if numbers[i] - numbers[i - 1] == 1 {
            delta_ones += 1;
        } else if numbers[i] - numbers[i - 1] == 3 {
            delta_threes += 1;
        }
    }
    delta_ones * delta_threes
}

fn solve_part2(numbers: &Vec<i64>) -> i64 {
    let mut combos = 1;
    let mut i = 0;
    while i < numbers.len() {
        let c = chain(&numbers[i..]);
        match c {
            0 | 1 => i += 1,
            2 => {
                combos *= 2;
                i += 2
            }
            3 => {
                combos *= 4;
                i += 3
            }
            4 => {
                combos *= 7;
                i += 4
            }
            x => {
                panic!("Whoops. Gots to do more work here: {}", x);
            }
        }
    }
    combos
}

fn chain(numbers: &[i64]) -> i64 {
    let mut current = 0;
    for i in 1..numbers.len() {
        if numbers[i] - numbers[i - 1] == 1 {
            current += 1;
        } else {
            return current;
        }
    }
    current
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let mut test_data: Vec<i64> = vec![16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4];
        test_data.sort_unstable();
        test_data.insert(0, 0);
        test_data.push(test_data.last().unwrap() + 3);
        assert_eq!(solve_part1(&mut test_data), 35);
        assert_eq!(solve_part2(&test_data), 8);

        let mut test_data2: Vec<i64> = vec![
            28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35,
            8, 17, 7, 9, 4, 2, 34, 10, 3,
        ];
        test_data2.sort_unstable();
        test_data2.insert(0, 0);
        test_data2.push(test_data2.last().unwrap() + 3);
        assert_eq!(solve_part1(&mut test_data2), 220);
        assert_eq!(solve_part2(&test_data2), 19208);
    }
}
