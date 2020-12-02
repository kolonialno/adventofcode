use regex::Regex;
use std::ops::Range;
#[derive(Debug)]
pub struct PasswordEntry {
    policy: PasswordPolicy,
    password: String,
}

#[derive(Debug)]
pub struct PasswordPolicy {
    chr: char,
    min: usize,
    max: usize,
}

impl PasswordEntry {
    fn is_valid(&self) -> bool {
        let num_chars = self
            .password
            .chars()
            .filter(|&c| c == self.policy.chr)
            .count();
        self.policy.range().contains(&num_chars)
    }
}

impl PasswordPolicy {
    fn range(&self) -> Range<usize> {
        self.min..(self.max + 1)
    }
}

pub fn parse(lines: Vec<String>) -> Vec<PasswordEntry> {
    let re = Regex::new(r"^(\d+)-(\d+) (\w): (\w+)$").unwrap();
    let mut result = Vec::new();

    for line in lines {
        let caps = re.captures(&line).expect("Line did not match regex");
        result.push(PasswordEntry {
            policy: PasswordPolicy {
                min: caps[1].parse().unwrap(),
                max: caps[2].parse().unwrap(),
                chr: caps[3].chars().next().unwrap(),
            },
            password: caps[4].to_string(),
        })
    }

    result
}

pub fn solve(entries: Vec<PasswordEntry>) -> usize {
    entries.into_iter().filter(|e| e.is_valid()).count()
}

#[cfg(test)]
mod tests {
    #[test]
    fn example() {
        let lines = vec![
            "1-3 a: abcde".to_string(),
            "1-3 b: cdefg".to_string(),
            "2-9 c: ccccccccc".to_string(),
        ];
        let entries = super::parse(lines);
        let result = super::solve(entries);
        assert_eq!(result, 2);
    }
}
