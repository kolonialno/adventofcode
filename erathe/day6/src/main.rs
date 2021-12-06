const DAYS: i32 = 256;
fn main() {
    let mut state: [i64; 9] = [0; 9];
    include_str!("../input.txt")
        .split(",")
        .map(|f| f.parse::<usize>().unwrap())
        .for_each(|f| state[f] += 1);

    for _ in 0..DAYS {
        state.rotate_left(1);
        state[6] += state[8];
    }

    println!("{}", state.iter().sum::<i64>());
}
