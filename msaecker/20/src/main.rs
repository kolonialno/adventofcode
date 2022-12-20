use std::fs;

use helpers::get_input_file;

fn new_pos(old_index: isize, element: isize, num_numbers: isize) -> usize {
    let mut new_pos = (old_index + element) % (num_numbers - 1);
    if new_pos <= 0 {
        new_pos += num_numbers - 1;
    }
    new_pos as usize
}

fn mix(numbers: &mut Vec<(usize, isize)>) {
    let num_numbers = numbers.len();
    // println!("{:?}", numbers.iter().map(|x| x.1).collect::<Vec<isize>>());
    for identifier in 0..num_numbers {
        let old_index = numbers.iter().position(|v| v.0 == identifier).unwrap();
        let (_, value) = numbers.remove(old_index);
        let new_index = new_pos(old_index as isize, value, num_numbers as isize);
        numbers.insert(new_index, (identifier, value));
        // println!("{:?}", numbers.iter().map(|x| x.1).collect::<Vec<isize>>());
    }
}

fn parse_numbers(input: &str) -> Vec<(usize, isize)> {
    input
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|s| s.parse::<isize>().unwrap())
        .enumerate()
        .collect::<Vec<(usize, isize)>>()
}

fn score(numbers: &[(usize, isize)]) -> isize {
    let zero_pos = numbers.iter().position(|(_, val)| *val == 0).unwrap();
    let i_pos = vec![zero_pos + 1000, zero_pos + 2000, zero_pos + 3000];
    i_pos.iter().map(|pos| numbers[pos % numbers.len()].1).sum()
}

fn get_decrypted_numbers(orig_numbers: &[(usize, isize)]) -> Vec<(usize, isize)> {
    const DECRYPTION_KEY: isize = 811589153;
    orig_numbers
        .iter()
        .map(|&(identifier, mut value)| {
            value *= DECRYPTION_KEY;
            (identifier, value)
        })
        .collect::<Vec<(usize, isize)>>()
}

fn main() {
    let input = fs::read_to_string(get_input_file()).unwrap();
    let mut numbers = parse_numbers(input.as_str());
    let orig_numbers = numbers.clone();
    mix(&mut numbers);
    println!("Mix 1: {}", score(&numbers));

    numbers = get_decrypted_numbers(&orig_numbers);
    for _ in 0..10 {
        mix(&mut numbers);
    }
    println!("Mix 10: {}", score(&numbers));
}

#[cfg(test)]
mod test {
    use std::{fs, path::PathBuf};

    use crate::{get_decrypted_numbers, mix, new_pos, parse_numbers, score};

    fn get_input() -> String {
        let mut test_file = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        test_file.push("test.txt");
        fs::read_to_string(test_file).unwrap()
    }

    #[test]
    fn part_1_example() {
        let input = get_input();
        let mut numbers = parse_numbers(input.as_str());
        mix(&mut numbers);
        let sum = score(&numbers);
        assert_eq!(sum, 3);
    }

    #[test]
    fn part_2_example() {
        let input = get_input();
        let mut numbers = parse_numbers(input.as_str());
        numbers = get_decrypted_numbers(&numbers);
        for _ in 0..10 {
            mix(&mut numbers);
        }
        let sum = score(&numbers);
        assert_eq!(sum, 1623178306);
    }

    #[test]
    fn new_pos_() {
        assert_eq!(new_pos(0, 1, 7), 1);
        assert_eq!(new_pos(6, 1, 7), 1);
        assert_eq!(new_pos(6, -1, 7), 5);
        assert_eq!(new_pos(5, 4, 7), 3);
    }

    #[test]
    fn new_pos_index_underflow() {
        assert_eq!(new_pos(0, -1, 7), 5);
        assert_eq!(new_pos(2, -2, 7), 6);
    }
}
