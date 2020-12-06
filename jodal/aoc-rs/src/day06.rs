use std::collections::HashMap;
use std::str::Lines;

use itertools::Itertools;

pub fn solve_a(group_answers: &Vec<GroupAnswer>) -> usize {
    group_answers.iter().map(|ga| ga.num_answers()).sum()
}

pub fn solve_b(group_answers: &Vec<GroupAnswer>) -> usize {
    group_answers
        .iter()
        .map(|ga| {
            ga.answers
                .iter()
                .filter(|(&_answer, &count)| count == ga.num_members)
                .count()
        })
        .sum()
}

#[derive(Debug)]
pub struct GroupAnswer {
    num_members: usize,
    answers: HashMap<char, usize>,
}

impl GroupAnswer {
    pub fn from_lines(lines: Lines) -> Vec<GroupAnswer> {
        let line_groups = lines.group_by(|line| !line.is_empty());
        line_groups
            .into_iter()
            .map(|(_, group)| {
                let group_lines: Vec<&str> = group.collect();
                let mut answers = HashMap::new();
                for c in group_lines.join("").chars() {
                    answers.insert(c, answers.get(&c).or(Some(&0)).unwrap() + 1);
                }
                GroupAnswer {
                    num_members: group_lines.len(),
                    answers: answers,
                }
            })
            .collect()
    }

    fn num_answers(&self) -> usize {
        self.answers.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_a() {
        let input = String::from(
            r"abc

a
b
c

ab
ac

a
a
a
a

b",
        );
        let group_answers = GroupAnswer::from_lines(input.lines());
        assert_eq!(solve_a(&group_answers), 11);
    }

    #[test]
    fn example_b() {
        let input = String::from(
            r"abc

a
b
c

ab
ac

a
a
a
a

b",
        );
        let group_answers = GroupAnswer::from_lines(input.lines());
        assert_eq!(solve_b(&group_answers), 6);
    }
}
