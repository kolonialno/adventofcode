use std::format;
use std::fs;

fn main() {
    let filename = "input.txt";
    let numbers = fs::read_to_string(filename)
        .expect(&format!("Failed to read file {}", filename))
        .lines()
        .map(|line| line.parse::<i64>().unwrap())
        .collect();
    let part1 = solve_part1(&numbers, 25);
    println!("Part1: {}", part1);
    println!(
        "Part2: {}",
        solve_part2(&numbers, numbers[0] + numbers[1], part1, 0, 1)
    );
}

fn solve_part1(numbers: &Vec<i64>, preamble_size: usize) -> i64 {
    for (i, num) in numbers.iter().enumerate() {
        if i < preamble_size {
            continue;
        }
        let mut valid = false;
        for x in i - preamble_size..i {
            for y in x + 1..i {
                if numbers[x] + numbers[y] == *num {
                    valid = true;
                }
            }
        }
        if !valid {
            return *num;
        }
    }
    -1
}

// Stolen with pride from @nnerik
fn solve_part2(numbers: &[i64], acc: i64, target: i64, left: usize, right: usize) -> i64 {
    if acc == target {
        *numbers[left..right].iter().min().unwrap() + *numbers[left..right].iter().max().unwrap()
    } else if acc < target {
        solve_part2(numbers, acc + numbers[right + 1], target, left, right + 1)
    } else {
        solve_part2(numbers, acc - numbers[left], target, left + 1, right)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let test_data: Vec<i64> = vec![
            35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309,
            576,
        ];
        assert_eq!(solve_part1(&test_data, 5), 127);
        assert_eq!(
            solve_part2(&test_data, test_data[0] + test_data[1], 127, 0, 1),
            62
        );
    }
}
