use std::collections::BinaryHeap;

pub fn solve_part1(input: &Vec<Vec<u32>>) -> u32 {
    input.iter()
        .map(|elf| elf.iter().sum())
        .max()
        .unwrap()
}

pub fn solve_part2(input: &Vec<Vec<u32>>) -> u32 {
    input.iter()
        .map(|elf| elf.iter().sum())
        .collect::<BinaryHeap<u32>>()
        .iter()
        .take(3)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_part1_returns_correct_answer() {
        let input: Vec<Vec<u32>> = vec![
            vec![
                1000,
                2000,
                3000,
            ],
            vec![
                4000,
            ],
            vec![
                5000,
                6000,
            ],
            vec![
                7000,
                8000,
                9000,
            ],
            vec![
                10000,
            ]
        ];

        let expected = 24000;

        assert_eq!(solve_part1(&input), expected);
    }

    #[test]
    fn solve_part2_returns_correct_answer() {
        let input: Vec<Vec<u32>> = vec![
            vec![
                1000,
                2000,
                3000,
            ],
            vec![
                4000,
            ],
            vec![
                5000,
                6000,
            ],
            vec![
                7000,
                8000,
                9000,
            ],
            vec![
                10000,
            ]
        ];

        let expected = 45000;

        assert_eq!(solve_part2(&input), expected);
    }
}
