use regex::Regex;
use std::str::Lines;

pub fn solve_a(entries: &Vec<PasswordEntry>) -> usize {
    entries.into_iter().filter(|e| e.is_valid_a()).count()
}

pub fn solve_b(entries: &Vec<PasswordEntry>) -> usize {
    entries.into_iter().filter(|e| e.is_valid_b()).count()
}

#[derive(Debug)]
pub struct PasswordEntry {
    policy: PasswordPolicy,
    password: String,
}

#[derive(Debug)]
pub struct PasswordPolicy {
    num1: usize,
    num2: usize,
    chr: char,
}

impl PasswordEntry {
    pub fn from_lines(lines: Lines) -> Vec<PasswordEntry> {
        let re = Regex::new(r"^(\d+)-(\d+) (\w): (\w+)$").unwrap();
        let mut result = Vec::new();

        for line in lines {
            let caps = re.captures(&line).expect("Line did not match regex");
            result.push(PasswordEntry {
                policy: PasswordPolicy {
                    num1: caps[1].parse().unwrap(),
                    num2: caps[2].parse().unwrap(),
                    chr: caps[3].chars().next().unwrap(),
                },
                password: caps[4].to_string(),
            })
        }

        result
    }

    fn is_valid_a(&self) -> bool {
        let num_chars = self
            .password
            .chars()
            .filter(|&chr| chr == self.policy.chr)
            .count();
        let range = self.policy.num1..(self.policy.num2 + 1);
        range.contains(&num_chars)
    }

    fn is_valid_b(&self) -> bool {
        let num_matching_chars = self
            .password
            .chars()
            .enumerate()
            .filter_map(|(index, chr)| {
                if (index + 1 == self.policy.num1) || (index + 1 == self.policy.num2) {
                    Some(chr)
                } else {
                    None
                }
            })
            .filter(|&chr| chr == self.policy.chr)
            .count();
        num_matching_chars == 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_a() {
        let input = String::from(
            "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc",
        );
        let entries = PasswordEntry::from_lines(input.lines());
        let result = solve_a(&entries);
        assert_eq!(result, 2);
    }

    #[test]
    fn example_b() {
        let input = String::from(
            "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc",
        );
        let entries = PasswordEntry::from_lines(input.lines());
        let result = solve_b(&entries);
        assert_eq!(result, 1);
    }
}
