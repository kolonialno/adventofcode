use std::str::Lines;

pub fn parse_numbers(lines: Lines) -> Vec<i32> {
    lines.map(|line| line.parse().unwrap()).collect()
}

pub fn solve_a(input: &[i32]) -> Option<i32> {
    for i in input {
        for j in input {
            if i + j == 2020 {
                return Some(i * j);
            }
        }
    }
    None
}

pub fn solve_b(input: &[i32]) -> Option<i32> {
    for i in input {
        for j in input {
            for k in input {
                if i + j + k == 2020 {
                    return Some(i * j * k);
                }
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    #[test]
    fn example_a() {
        let input = vec![1721, 979, 366, 299, 675, 1456];
        let result = super::solve_a(&input);
        assert_eq!(result.unwrap(), 514_579);
    }

    #[test]
    fn example_b() {
        let input = vec![1721, 979, 366, 299, 675, 1456];
        let result = super::solve_b(&input);
        assert_eq!(result.unwrap(), 241_861_950);
    }
}
