use std::format;
use std::fs;
use std::str::Lines;

fn main() {
    let filename = "input.txt";
    let contents =
        fs::read_to_string(filename).expect(&format!("Failed to read file {}", filename));
    println!("Part1: {}", solve_part1(contents.lines()));
    println!("Part2: {}", solve_part2(contents.lines()));
}

fn solve_part1(input: Lines) -> u16 {
    input.map(|line| seat_id(line)).max().unwrap()
}

fn solve_part2(input: Lines) -> u16 {
    let seats = input.map(|line| seat_id(line)).collect::<Vec<u16>>();
    let seat_before_mine = *seats
        .iter()
        .find(|&e| seats.contains(&(e + 2)) && !seats.contains(&(e + 1)))
        .unwrap();
    seat_before_mine + 1
}

fn seat_id(input: &str) -> u16 {
    let row_binary_string = &input.replace("F", "0").replace("B", "1")[0..7];
    let row = u16::from_str_radix(row_binary_string, 2).unwrap();
    let col_binary_string = &input.replace("L", "0").replace("R", "1")[7..];
    let col = u16::from_str_radix(col_binary_string, 2).unwrap();
    row * 8 + col
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        assert_eq!(seat_id("FBFBBFFRLR"), 357);
        assert_eq!(seat_id("BFFFBBFRRR"), 567);
        assert_eq!(seat_id("FFFBBBFRRR"), 119);
        assert_eq!(seat_id("BBFFBBFRLL"), 820);
    }
}
