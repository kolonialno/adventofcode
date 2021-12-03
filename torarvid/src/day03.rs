use crate::util::file_by_lines;

pub fn run() {
    let numbers = file_by_lines("day03.txt");
    let len = numbers[0].len();

    let mut gamma_str = "".to_owned();
    let mut epsilon_str = "".to_owned();
    for i in 0..len {
        let (zeros, ones) = count_ones_and_zeros_at_digit(&numbers, i);
        gamma_str.push_str(if ones > zeros {"1"} else {"0"});
        epsilon_str.push_str(if ones > zeros {"0"} else {"1"});
    }

    let gamma = u64::from_str_radix(&gamma_str, 2).unwrap();
    let epsilon = u64::from_str_radix(&epsilon_str, 2).unwrap();

    println!("Part 1: {}", gamma * epsilon);

    // Part 2
    let mut ox_numbers = numbers.clone();
    let mut co2_numbers = numbers.clone();
    for i in 0..len {
        if ox_numbers.len() > 1 {
            ox_numbers = filter_numbers(&ox_numbers, i, '0', '1');
        }
        if co2_numbers.len() > 1 {
            co2_numbers = filter_numbers(&co2_numbers, i, '1', '0');
        }
    }
    let ox = u64::from_str_radix(&ox_numbers[0], 2).unwrap();
    let co2 = u64::from_str_radix(&co2_numbers[0], 2).unwrap();
    println!("Part 2: {}", ox * co2);
}

fn count_ones_and_zeros_at_digit(numbers: &Vec<String>, digit: usize) -> (usize, usize) {
    let mut zeros = 0;
    let mut ones = 0;
    for line in numbers {
        let digit = line.chars().nth(digit).unwrap();
        if digit == '0' { zeros += 1 } else { ones += 1 }
    }
    (zeros, ones)
}

fn filter_numbers(numbers: &Vec<String>, digit: usize, x: char, y: char) -> Vec<String> {
    let mut filtered = Vec::new();
    let (zeros, ones) = count_ones_and_zeros_at_digit(numbers, digit);
    for line in numbers {
        let ch = line.chars().nth(digit).unwrap();
        let oxygen_match = ch == x && ones >= zeros;
        let co2_match = ch == y && zeros > ones;
        if oxygen_match || co2_match {
            filtered.push(line.clone());
        }
    }
    filtered
}
