fn main() {
    let input = include_str!("../input.txt");

    println!("Part one: {}", part_one(input));
    println!("Part two: {}", part_two(input));
}

fn part_one(input: &str) -> usize {
    input
        .lines()
        .filter_map(|line| {
            let chars = line.chars();
            let first = chars.clone().find(|&c| c.is_numeric())?;
            let last = chars.clone().rfind(|&c| c.is_numeric())?;
            let num = format!("{}{}", first, last).parse::<usize>().ok()?;
            return Some(num);
        })
        .sum()
}

static DIGIT_NAMES: [&str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

fn find_digit(substring: &str) -> Option<usize> {
    // Check for numeric digit
    let first_char = substring.chars().next()?;
    if first_char.is_numeric() {
        return first_char.to_digit(10).map(|n| n.try_into().ok())?;
    }

    // Check for digit name
    for (i, &name) in DIGIT_NAMES.iter().enumerate() {
        if substring.starts_with(name) {
            return Some(i + 1);
        }
    }

    None
}

fn part_two(input: &str) -> usize {
    input
        .lines()
        .filter_map(|line| {
            let mut first = None;
            let mut last = None;

            // Loop through each substring of the line and look for digits
            for i in 0..line.len() {
                let substring = &line[i..];
                if let Some(digit) = find_digit(substring) {
                    if let None = first {
                        first = Some(digit);
                    } else {
                        last = Some(digit);
                    }
                }
            }

            // This tripped me up
            if let None = last {
                last = first;
            }

            if let (Some(first), Some(last)) = (first, last) {
                let num = format!("{}{}", first, last)
                    .parse::<usize>()
                    .expect("this should definitely be a number");

                Some(num)
            } else {
                None
            }
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sample_part_one() {
        let sample = include_str!("../sample-1.txt");
        assert_eq!(part_one(sample), 142);
    }

    #[test]
    fn sample_part_two() {
        let sample = include_str!("../sample-2.txt");
        assert_eq!(part_two(sample), 281);
    }
}
