fn round_value(action: &str) -> u32 {
    match action {
        "A X" => 4,
        "A Y" => 8,
        "A Z" => 3,
        "B X" => 1,
        "B Y" => 5,
        "B Z" => 9,
        "C X" => 7,
        "C Y" => 2,
        "C Z" => 6,
        _ => 0,
    }
}

fn transform_round(action: &str) -> &str {
    match action {
        "A X" => "A Z",
        "A Y" => "A X",
        "A Z" => "A Y",
        "B X" => "B X",
        "B Y" => "B Y",
        "B Z" => "B Z",
        "C X" => "C Y",
        "C Y" => "C Z",
        "C Z" => "C X",
        _ => "",
    }
}

pub fn solve_part1(input: &Vec<&str>) -> u32 {
    input.iter()
        .fold(0, |acc, round| acc + round_value(round))
}

pub fn solve_part2(input: &Vec<&str>) -> u32 {
    input.iter()
        .fold(0, |acc, round| acc + round_value(transform_round(round)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_part1_returns_correct_answer() {
        let input = vec![
            "A Y",
            "B X",
            "C Z",
        ];

        let expected = 15;

        assert_eq!(solve_part1(&input), expected);
    }

    #[test]
    fn solve_part2_returns_correct_answer() {
        let input = vec![
            "A Y",
            "B X",
            "C Z",
        ];

        let expected = 12;

        assert_eq!(solve_part2(&input), expected);
    }
}
