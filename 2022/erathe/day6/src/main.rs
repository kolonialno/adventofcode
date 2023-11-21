use itertools::Itertools;

const P: usize = 4;
const M: usize = 14;

fn main() {
    let input = include_str!("../input.txt");

    println!("part 1: {}", unique_window(input, P));
    println!("part 2: {}", unique_window(input, M));
}

fn unique_window(inp: &str, size: usize) -> usize {
    for i in 0..inp.len() {
        if inp[i..i + size].chars().all_unique() {
            return i + size;
        }
    }
    0
}
