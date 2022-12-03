use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn priority(c: char) -> u32 {
    if c.is_lowercase() {
        c as u32 - 96
    } else {
        c as u32 - 38
    }
}

fn check_compartments(lhs: &str, rhs: &str) -> u32 {
    for c1 in lhs.chars() {
        if rhs.find(c1).is_some() {
            return priority(c1);
        }
    }
    panic!("No common items");
}

fn find_badge(group: &[String]) -> char {
    for c1 in group[0].chars() {
        if group[1].find(c1).is_some() && group[2].find(c1).is_some() {
            return c1;
        }
    }
    panic!("No common badge");
}

fn main() {
    const INPUT_FILE: &str = "input.txt";
    let file = File::open(INPUT_FILE).unwrap();
    let lines = BufReader::new(file).lines();

    // part 1
    let mut total_score = 0;
    for line in lines.flatten() {
        let (lhs, rhs) = line.split_at(line.len() / 2);
        total_score += check_compartments(lhs, rhs);
    }
    println!("Sum of priorities: {}", total_score);

    // part2
    let mut badge_sum = 0;
    let file = File::open(INPUT_FILE).unwrap();
    let lines = BufReader::new(file)
        .lines()
        .flatten()
        .collect::<Vec<String>>();
    let elf_groups = lines.chunks_exact(3);
    for group in elf_groups {
        badge_sum += priority(find_badge(group));
    }
    println!("Badge priorities: {}", badge_sum);
}

#[cfg(test)]
mod tests {
    use crate::priority;

    #[test]
    fn priority_() {
        assert_eq!(1, priority('a'));
        assert_eq!(16, priority('p'));
        assert_eq!(19, priority('s'));
        assert_eq!(20, priority('t'));
        assert_eq!(22, priority('v'));
        assert_eq!(26, priority('z'));
        assert_eq!(27, priority('A'));
        assert_eq!(38, priority('L'));
        assert_eq!(42, priority('P'));
        assert_eq!(52, priority('Z'));
    }
}
