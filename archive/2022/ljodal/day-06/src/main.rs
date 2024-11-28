// Check if the 4 characters are a start of package marker, meaning all four characters are unique.
fn is_start_of_package(chars: &&[char], window_size: usize) -> bool {
    if !chars.len() == window_size {
        return false;
    }

    for (i, c) in chars.iter().enumerate() {
        for j in 0..window_size {
            if i == j {
                continue;
            }

            if chars[j] == *c {
                return false;
            }
        }
    }

    true
}
fn solve(input: &str, window_size: usize) -> usize {
    let chars: Vec<char> = input.chars().collect();
    chars
        .windows(window_size)
        .enumerate()
        .filter(|(_, window)| is_start_of_package(window, window_size))
        .map(|(i, _)| i + window_size)
        .next()
        .expect("Expected at least one section of four uniq chars")
}

fn solve_part1(input: &str) -> usize {
    solve(input, 4)
}

fn solve_part2(input: &str) -> usize {
    solve(input, 14)
}

fn main() {
    let input = include_str!("./input.txt");
    let sum = solve_part1(input);
    println!("Part 1: {sum}");
    let sum = solve_part2(input);
    println!("Part 2: {sum}");
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_part1 {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                println!("Input: {input}\nExpected: {expected:?}");
                assert_eq!(solve_part1(input), expected);
            }
        )*
        }
    }

    test_part1! {
        solve_part1_1: ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7),
        solve_part1_2: ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
        solve_part1_3: ("nppdvjthqldpwncqszvftbrmjlhg", 6),
        solve_part1_4: ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
        solve_part1_5: ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11),
    }

    macro_rules! test_part2 {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                println!("Input: {input}\nExpected: {expected:?}");
                assert_eq!(solve_part2(input), expected);
            }
        )*
        }
    }

    test_part2! {
        solve_part2_1: ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
        solve_part2_2: ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
        solve_part2_3: ("nppdvjthqldpwncqszvftbrmjlhg", 23),
        solve_part2_4: ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
        solve_part2_5: ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26),
    }
}
