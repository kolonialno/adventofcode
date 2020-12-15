use std::collections::HashMap;
use std::fs;

fn main() {
    let content = fs::read_to_string("input.txt").expect("Failed to read input.txt");
    println!("Part1: {}", solve_part1(&content));
    println!("Part2: {}", solve_part2(&content));
}

fn solve_part1(input: &str) -> u64 {
    let mut mask_or: u64 = 0;
    let mut mask_and: u64 = 0;
    let mut map = HashMap::new();
    for line in input.lines() {
        if line.starts_with("mask = ") {
            mask_or = 0;
            mask_and = 0;
            let mask_raw = &line[7..];
            for (i, c) in mask_raw.chars().rev().enumerate() {
                match c {
                    '1' => mask_or |= 1 << i,
                    '0' => mask_and |= 1 << i,
                    _ => (),
                }
            }
            mask_and = !mask_and;
        } else {
            let value = line.split(" = ").nth(1).unwrap().parse::<u64>().unwrap();
            let addr = line[4..].split("]").nth(0).unwrap().parse::<u64>().unwrap();
            map.insert(addr, value & mask_and | mask_or);
        }
    }
    map.values().sum()
}

fn solve_part2(input: &str) -> u64 {
    let mut mask_raw: &str = "";
    let mut map = HashMap::new();
    for line in input.lines() {
        if line.starts_with("mask = ") {
            mask_raw = &line[7..];
        } else {
            let value = line.split(" = ").nth(1).unwrap().parse::<u64>().unwrap();
            let addr = line[4..].split("]").nth(0).unwrap().parse::<u64>().unwrap();
            for modified_addr in mask_variants(mask_raw, addr) {
                map.insert(modified_addr, value);
            }
        }
    }
    map.values().sum()
}

fn mask_variants(mask_raw: &str, addr: u64) -> Vec<u64> {
    let x_count = mask_raw.matches("X").count();
    let addr_binary_string = &format!("{:0>36b}", addr);
    let mut modified_mask = String::from(mask_raw);
    for (i, c) in addr_binary_string.chars().enumerate() {
        if c == '1' && modified_mask.chars().nth(i) == Some('0') {
            modified_mask.replace_range(i..i + 1, "1");
        }
    }
    let mut variants = Vec::new();
    for i in 0..1 << x_count {
        let i_binary_string = &format!("{:0>36b}", i)[36 - x_count..];
        let mut mask = modified_mask.clone();
        for c in i_binary_string.chars() {
            mask = mask.replacen("X", &c.to_string(), 1);
        }
        variants.push(u64::from_str_radix(&mask, 2).unwrap());
    }
    variants
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let test_data = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0";
        assert_eq!(solve_part1(&test_data), 165);
        let test_data2 = "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1";
        assert_eq!(solve_part2(&test_data2), 208);
    }
}
