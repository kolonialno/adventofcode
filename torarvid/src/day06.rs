use crate::util::file_by_lines;

pub fn run() {
    let timings = &file_by_lines("day06.txt")[0];
    let timings: Vec<u64> = timings
        .split(",")
        .map(|x| x.parse::<u64>().unwrap())
        .collect();

    println!("Part 1: {}", solve(&timings, 80));
    println!("Part 2: {}", solve(&timings, 256));
}

fn solve(timings: &Vec<u64>, iterations: u64) -> u64 {
    let mut arr: [u64; 9] = [0; 9];
    for timing in timings {
        arr[*timing as usize] += 1;
    }

    for _ in 0..iterations {
        arr.rotate_left(1);
        arr[6] += arr[8];
    }
    arr.iter().sum()
}
