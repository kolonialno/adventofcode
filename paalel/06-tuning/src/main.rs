use std::collections::HashSet;

fn find_marker(string: &str, window_size: usize) -> i32 {
    let chars: Vec<char> = string.chars().collect();

    for i in 0..chars.len() - window_size {
        let set: HashSet<&char> = chars[i..(i + window_size)].iter().collect();
        if set.len() == window_size {
            return (i + window_size).try_into().unwrap();
        }
    }

    -1
}
fn main() {
    println!("Hello, day 6!");

    #[rustfmt::skip]
    assert_eq!(find_marker(&"bvwbjplbgvbhsrlpgdmjqwftvncz".to_string(), 4), 5);
    #[rustfmt::skip]
    assert_eq!(find_marker(&"nppdvjthqldpwncqszvftbrmjlhg:".to_string(), 4), 6);
    #[rustfmt::skip]
    assert_eq!( find_marker(&"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".to_string(), 4), 10);
    #[rustfmt::skip]
    assert_eq!( find_marker(&"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".to_string(), 4), 11);

    let input = include_str!("../input.txt");
    println!("Solution to problem 1:{}", find_marker(&input, 4));

    #[rustfmt::skip]
    assert_eq!(find_marker(&"mjqjpqmgbljsphdztnvjfqwrcgsmlb".to_string(), 14),19);
    #[rustfmt::skip]
    assert_eq!(find_marker(&"bvwbjplbgvbhsrlpgdmjqwftvncz".to_string(), 14),23);
    #[rustfmt::skip]
    assert_eq!(find_marker(&"nppdvjthqldpwncqszvftbrmjlhg".to_string(), 14),23);
    #[rustfmt::skip]
    assert_eq!(find_marker(&"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".to_string(), 14),29);
    #[rustfmt::skip]
    assert_eq!(find_marker(&"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".to_string(), 14),26);

    println!("Solution to problem 2:{}", find_marker(&input, 14));
}
