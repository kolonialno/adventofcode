use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::fs;

fn main() {
    let content = fs::read_to_string("input.txt").expect("Failed to read input.txt");
    println!("Part1: {}", solve_part1(&content));
    println!("Part2: {}", solve_part2(&content));
}

fn solve_part1(input: &str) -> i64 {
    let timestamp = input.lines().nth(0).unwrap().parse::<i64>().unwrap();
    let buses_raw = input.lines().nth(1).unwrap();
    let buses: Vec<i64> = buses_raw
        .split(",")
        .filter(|b| b != &"x")
        .map(|b| b.parse::<i64>().unwrap())
        .collect();
    let earliest_departure = |bus: &&i64| timestamp + **bus - timestamp % **bus;
    let min_bus = buses.iter().min_by_key(earliest_departure).unwrap();
    let wait_time = earliest_departure(&&min_bus) - timestamp;
    min_bus * wait_time
}

fn solve_part2(input: &str) -> i64 {
    let buses_raw = input.lines().nth(1).unwrap();
    let buses: Vec<(i64, i64)> = buses_raw
        .split(",")
        .enumerate()
        .map(|(i, bus)| (i as i64, bus.parse::<i64>()))
        .filter(|(_, bus)| bus.is_ok())
        .map(|(i, bus)| (i, bus.unwrap()))
        .collect();
    let mut result = buses[0];
    for i in 1..buses.len() {
        result = combine_phased_rotations(&result, &buses[i]);
    }
    result.0
}

// Stealing this one from here:
// https://math.stackexchange.com/a/3864593/861794
fn combine_phased_rotations(a: &(i64, i64), b: &(i64, i64)) -> (i64, i64) {
    let (gcd, s, _) = extended_gcd(&a.1, &b.1);
    let offset_diff = a.0 + b.0;
    let pd_mult = BigInt::from(offset_diff / gcd);
    let pd_rem = offset_diff % gcd;
    if pd_rem != 0 {
        panic!("These will never sync");
    }
    let combined_period = a.1 / gcd * b.1;
    let big_s = BigInt::from(s);
    let combined_phase =
        (combined_period + ((a.0 - big_s * pd_mult * a.1) % combined_period)) % combined_period;
    (combined_phase.to_i64().unwrap(), combined_period)
}

fn extended_gcd(a: &i64, b: &i64) -> (i64, i64, i64) {
    let mut s = 0;
    let mut r = *b;
    let mut old_s = 1;
    let mut old_r = *a;
    while r != 0 {
        let q = old_r / r;
        let temp = r;
        r = old_r - q * r;
        old_r = temp;
        let temp = s;
        s = old_s - q * s;
        old_s = temp;
    }
    let bez_t = if *b != 0 { (old_r - old_s * a) / b } else { 0 };
    (old_r, old_s, bez_t)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let test_data = "939\n7,13,x,x,59,x,31,19";
        assert_eq!(solve_part1(&test_data), 295);
        assert_eq!(solve_part2(&test_data), 1068781);
        assert_eq!(solve_part2("\n17,x,13,19"), 3417);
        assert_eq!(solve_part2("\n67,7,59,61"), 754018);
        assert_eq!(solve_part2("\n67,x,7,59,61"), 779210);
        assert_eq!(solve_part2("\n67,7,x,59,61"), 1261476);
        assert_eq!(solve_part2("\n1789,37,47,1889"), 1202161486);
    }
}
