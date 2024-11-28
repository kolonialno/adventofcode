use std::{collections::VecDeque, fs};

use helpers::get_input_file;
use itertools::Itertools;

fn start_of_sequence(input: String, distinct_chars: usize) -> (usize, String) {
    let mut chars = VecDeque::from_iter(input.chars().take(distinct_chars));
    for (i, c) in input.chars().enumerate().skip(distinct_chars) {
        if chars.iter().unique().count() == distinct_chars {
            return (i, chars.iter().collect::<String>());
        }
        chars.pop_front();
        chars.push_back(c);
    }
    panic!("No distinct Sequence!");
}

fn main() {
    let input = fs::read_to_string(get_input_file()).unwrap();
    let (packet_start, packet_seq) = start_of_sequence(input.clone(), 4);
    println!(
        "Packet position: {}, sequence: {}",
        packet_start, packet_seq
    );
    let (message_start, message_seq) = start_of_sequence(input, 14);
    println!(
        "Message position: {}, sequence: {}",
        message_start, message_seq
    );
}
