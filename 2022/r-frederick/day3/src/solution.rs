use std::collections::HashSet;

fn to_value(c: char) -> u32 {
    match u8::try_from(c) {
        Ok(x) if x >= 97 => x as u32 - 96,
        Ok(x) => x as u32 - 38,
        _ => 0,
    }
}

type Sack = HashSet<char>;

fn to_sack(a: &str) -> Sack {
    HashSet::from_iter(a.chars())
}

fn sack_value(s: Sack) -> u32 {
    s.iter().fold(0, |acc, c| acc + to_value(*c))
}

fn intersection(a: &Sack, b: &Sack) -> Sack {
    a & b
}

pub fn solve_part1(input: &Vec<&str>) -> u32 {
    input
        .iter()
        .map(|l| {
            let (a, b) = l.split_at(l.len() / 2);
            sack_value(intersection(&to_sack(a), &to_sack(b)))
        })
        .sum()
}

pub fn solve_part2(input: &Vec<&str>) -> u32 {
    input
        .chunks(3)
        .map(|l| {
            sack_value(l[1..3].iter().fold(to_sack(l[0]), |acc, s| {
                intersection(&to_sack(s), &acc)
            }))
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_part1_returns_correct_answer() {
        let input: Vec<&str> = vec![
            "vJrwpWtwJgWrhcsFMMfFFhFp",
            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
            "PmmdzqPrVvPwwTWBwg",
            "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
            "ttgJtRGJQctTZtZT",
            "CrZsJsPPZsGzwwsLwLmpwMDw",
        ];

        let expected = 157;

        assert_eq!(solve_part1(&input), expected);
    }

    #[test]
    fn solve_part2_returns_correct_answer() {
        let input: Vec<&str> = vec![
            "vJrwpWtwJgWrhcsFMMfFFhFp",
            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
            "PmmdzqPrVvPwwTWBwg",
            "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
            "ttgJtRGJQctTZtZT",
            "CrZsJsPPZsGzwwsLwLmpwMDw",
        ];

        let expected = 70;

        assert_eq!(solve_part2(&input), expected);
    }
}
