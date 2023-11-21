use std::fs;

use helpers::get_input_file;

fn main() {
    let input = fs::read_to_string(get_input_file()).unwrap();
    println!("Input: {}", input);
}
