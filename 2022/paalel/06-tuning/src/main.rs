use std::collections::HashSet;

fn find_marker(string: &str, window_size: usize) -> usize {
    let chars: Vec<char> = string.chars().collect();

    for i in 0..(chars.len() - window_size) {
        let set: HashSet<&char> = chars[i..(i + window_size)].iter().collect();
        if set.len() == window_size {
            return i + window_size;
        }
    }

    0
}

fn main() {
    println!("Hello, day 6!");

    let input = include_str!("../input.txt");
    println!("Solution to problem 1:{}", find_marker(&input, 4));
    println!("Solution to problem 2:{}", find_marker(&input, 14));
}

#[cfg(test)]
mod tests {
    use super::find_marker;

    #[test]
    fn window_size_4() {
        let test_cases: [(&str, usize); 4] = [
            ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
            ("nppdvjthqldpwncqszvftbrmjlhg:", 6),
            ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
            ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11),
        ];

        for (input, result) in test_cases {
            assert_eq!(find_marker(&input, 4), result)
        }
    }

    #[test]
    fn window_size_14() {
        let test_cases: [(&str, usize); 5] = [
            ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
            ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
            ("nppdvjthqldpwncqszvftbrmjlhg", 23),
            ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
            ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26),
        ];

        for (input, result) in test_cases {
            assert_eq!(find_marker(&input, 14), result)
        }
    }
}
