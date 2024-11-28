use std::str::FromStr;

#[derive(Debug)]
struct SectionAssignment(u32, u32);

impl SectionAssignment {
    fn contains(&self, other: &SectionAssignment) -> bool {
        self.0 <= other.0 && self.1 >= other.1
    }

    fn overlaps(&self, other: &SectionAssignment) -> bool {
        (self.0 >= other.0 && self.0 <= other.1) || (self.1 >= other.0 && self.1 <= other.1)
    }
}

impl FromStr for SectionAssignment {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((start, end)) = s.split_once('-') {
            match (start.parse::<u32>(), end.parse::<u32>()) {
                (Ok(s), Ok(e)) => return Ok(SectionAssignment(s, e)),
                (_, _) => {
                    return Err(
                        format!("Invalid section assignment specification provided: {}", s),
                    )
                }
            }
        }

        Err(format!("Invalid section assignment specification provided: {}", s))
    }
}

pub fn solve_part1(input: &[Vec<&str>]) -> u32 {
    input.iter().fold(0, |acc, l| {
        let first = SectionAssignment::from_str(l[0]).unwrap();
        let second = SectionAssignment::from_str(l[1]).unwrap();

        if first.contains(&second) || second.contains(&first) {
            return acc + 1;
        }

        acc
    })
}

pub fn solve_part2(input: &[Vec<&str>]) -> u32 {
    input.iter().fold(0, |acc, l| {
        let first = SectionAssignment::from_str(l[0]).unwrap();
        let second = SectionAssignment::from_str(l[1]).unwrap();

        if first.overlaps(&second) || second.overlaps(&first) {
            return acc + 1;
        }

        acc
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_part1_returns_correct_answer() {
        let input: Vec<Vec<&str>> = vec![
            vec!["2-4", "6-8"],
            vec!["2-3", "4-5"],
            vec!["5-7", "7-9"],
            vec!["2-8", "3-7"],
            vec!["6-6", "4-6"],
            vec!["2-6", "4-8"],
        ];

        let expected = 2;

        assert_eq!(solve_part1(&input), expected);
    }

    #[test]
    fn solve_part2_returns_correct_answer() {
        let input: Vec<Vec<&str>> = vec![
            vec!["2-4", "6-8"],
            vec!["2-3", "4-5"],
            vec!["5-7", "7-9"],
            vec!["2-8", "3-7"],
            vec!["6-6", "4-6"],
            vec!["2-6", "4-8"],
        ];

        let expected = 4;

        assert_eq!(solve_part2(&input), expected);
    }
}
