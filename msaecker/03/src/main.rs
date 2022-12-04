use std::{
    collections::HashSet,
    fs::File,
    io::{BufRead, BufReader},
};

fn priority(c: char) -> u32 {
    match c {
        'a'..='z' => c as u32 - 96,
        'A'..='Z' => c as u32 - 38,
        _ => panic!("Unknown input!"),
    }
}

fn check_compartments(lhs: &str, rhs: &str) -> u32 {
    let h0 = lhs.chars().collect::<HashSet<char>>();
    priority(rhs.chars().find(|c| h0.contains(c)).unwrap())
}

fn find_badge(group: &[String]) -> char {
    let h0 = group[0].chars().collect::<HashSet<char>>();
    let h1 = group[1].chars().collect::<HashSet<char>>();
    group[2]
        .chars()
        .find(|c| h0.contains(c) && h1.contains(c))
        .unwrap()
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
    for group in lines.chunks_exact(3) {
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
