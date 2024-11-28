use std::collections::HashMap;

use crate::util::file_by_lines;

struct Chunk {
    content: String,
}

impl Chunk {
    fn syntax_error_score(&self) -> u64 {
        let mut points = HashMap::new();
        points.insert(')', 3u64);
        points.insert(']', 57u64);
        points.insert('}', 1197u64);
        points.insert('>', 25137u64);

        let mut score = 0;
        let mut check_stack = |stack_char, point_char, expected| match stack_char {
            None => panic!("empty stack"),
            Some(ch) => {
                if ch != expected {
                    score += points.get(&point_char).unwrap();
                }
            }
        };

        let mut stack = Vec::new();
        for c in self.content.chars() {
            match c {
                '{' | '(' | '[' | '<' => stack.push(c),
                '}' => check_stack(stack.pop(), c, '{'),
                ')' => check_stack(stack.pop(), c, '('),
                ']' => check_stack(stack.pop(), c, '['),
                '>' => check_stack(stack.pop(), c, '<'),
                _ => {}
            }
        }
        score
    }

    fn autocomplete_score(&self) -> Option<u64> {
        let mut points = HashMap::new();
        points.insert(')', 1u64);
        points.insert(']', 2u64);
        points.insert('}', 3u64);
        points.insert('>', 4u64);

        let mut stack = Vec::new();
        let is_bad_char = |stack_char, expected| match stack_char {
            None => panic!("empty stack"),
            Some(ch) => ch != expected,
        };
        let corrupted = self.content.chars().any(|c| match c {
            '{' | '(' | '[' | '<' => {
                stack.push(c);
                false
            }
            '}' => is_bad_char(stack.pop(), '{'),
            ')' => is_bad_char(stack.pop(), '('),
            ']' => is_bad_char(stack.pop(), '['),
            '>' => is_bad_char(stack.pop(), '<'),
            _ => false,
        });
        if corrupted {
            return None;
        }

        let mut score = 0;
        while stack.len() > 0 {
            score = score * 5
                + match stack.pop() {
                    Some('{') => points.get(&'}').unwrap(),
                    Some('(') => points.get(&')').unwrap(),
                    Some('[') => points.get(&']').unwrap(),
                    Some('<') => points.get(&'>').unwrap(),
                    _ => &0,
                }
        }
        Some(score)
    }
}

pub fn run() {
    let lines = &file_by_lines("day10.txt");
    let chunks = lines
        .iter()
        .map(|line| Chunk {
            content: line.to_string(),
        })
        .collect::<Vec<_>>();

    let score = &chunks
        .iter()
        .map(|chunk| chunk.syntax_error_score())
        .sum::<u64>();
    println!("Part 1: {}", score);

    let mut scores = chunks
        .iter()
        .flat_map(|chunk| chunk.autocomplete_score())
        .collect::<Vec<_>>();
    scores.sort();
    println!("Part 2: {}", scores[(scores.len() - 1) / 2]);
}
