use regex::Regex;

#[derive(Debug)]
pub struct PasswordEntry {
    policy: PasswordPolicy,
    password: String,
}

#[derive(Debug)]
pub struct PasswordPolicy {
    chr: char,
    pos1: usize,
    pos2: usize,
}

impl PasswordEntry {
    fn is_valid(&self) -> bool {
        let num_matching_chars = self
            .password
            .chars()
            .enumerate()
            .filter_map(|(index, chr)| {
                if (index + 1 == self.policy.pos1) || (index + 1 == self.policy.pos2) {
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

pub fn parse(lines: Vec<String>) -> Vec<PasswordEntry> {
    let re = Regex::new(r"^(\d+)-(\d+) (\w): (\w+)$").unwrap();
    let mut result = Vec::new();

    for line in lines {
        let caps = re.captures(&line).expect("Line did not match regex");
        result.push(PasswordEntry {
            policy: PasswordPolicy {
                pos1: caps[1].parse().unwrap(),
                pos2: caps[2].parse().unwrap(),
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
            "1-3 a: abcde".to_string(),     // Valid
            "1-3 b: cdefg".to_string(),     // Invalid
            "2-9 c: ccccccccc".to_string(), // Invalid
        ];
        let entries = super::parse(lines);
        let result = super::solve(entries);
        assert_eq!(result, 1);
    }
}
