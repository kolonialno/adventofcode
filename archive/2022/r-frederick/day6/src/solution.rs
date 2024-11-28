use std::collections::HashSet;

pub fn solve(input: &str, start_size: usize) -> usize {
    input.split("")
        .collect::<Vec<&str>>()
        .windows(start_size)
        .enumerate()
        .skip(1)
        .find(|(_i, w)| {
            let set: HashSet<&&str> = HashSet::from_iter(w.iter());

            set.len() == start_size 
        })
        .unwrap()
        .0
        + start_size - 1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_part1_returns_correct_answer() {
        let examples = vec![
            ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
            ("nppdvjthqldpwncqszvftbrmjlhg", 6),
            ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
            ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11),
        ];

        for (input, expected) in examples {
            assert_eq!(solve(&input, 4), expected);
        }
    }

    #[test]
    fn solve_part2_returns_correct_answer() {
        let examples = vec![
            ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
            ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
            ("nppdvjthqldpwncqszvftbrmjlhg", 23),
            ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
            ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26),
        ];

        for (input, expected) in examples {
            assert_eq!(solve(&input, 14), expected);
        }
    }
}
