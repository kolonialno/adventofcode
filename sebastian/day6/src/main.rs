fn main() {
    let input = include_str!("../input.txt");
    println!("Signal starts at {}", len_to_marker(input, 4));
    println!("Message starts at {}", len_to_marker(input, 14));
}

fn all_different(buf: &Vec<char>) -> bool {
    for (i, c) in buf.iter().enumerate() {
        if buf[i+1..].contains(c) {
            return false;
        }
    }
    
    true
}

fn len_to_marker(input: &str, num_distinct: usize) -> usize {
    let mut buf = vec![];
    for (i, c) in input.chars().enumerate() {
        buf.insert(0, c);
        if buf.len() <= num_distinct {
            continue;
        }
        
        buf.pop();
        
        if all_different(&buf) { return i + 1; }
    }
    panic!("Couldn't find start of signal");
}

#[cfg(test)]
mod test {
    use crate::len_to_marker;

    #[test]
    fn test_check_for_marker() {
        assert_eq!(len_to_marker("bvwbjplbgvbhsrlpgdmjqwftvncz", 4), 5);
        assert_eq!(len_to_marker("nppdvjthqldpwncqszvftbrmjlhg", 4), 6);
        assert_eq!(len_to_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4), 7);
        assert_eq!(len_to_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4), 10);
        assert_eq!(len_to_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4), 11);
    }
    
    #[test]
    fn test_check_for_message_marker() {
        assert_eq!(len_to_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14), 19);
        assert_eq!(len_to_marker("bvwbjplbgvbhsrlpgdmjqwftvncz", 14), 23);
        assert_eq!(len_to_marker("nppdvjthqldpwncqszvftbrmjlhg", 14), 23);
        assert_eq!(len_to_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14), 29);
        assert_eq!(len_to_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14), 26);
    }
}