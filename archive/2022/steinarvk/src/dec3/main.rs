use itertools::Itertools;
use std::collections::HashSet;

type Result<T> = std::result::Result<T, anyhow::Error>;

fn char_value(x: char) -> i32 {
    if ('a'..='z').contains(&x) {
        (x as i32) - ('a' as i32) + 1
    } else if ('A'..='Z').contains(&x) {
        (x as i32) - ('A' as i32) + 27
    } else {
        panic!("no value specified for {}", x)
    }
}

fn main() -> Result<()> {
    let mut first_running_total = 0;
    let mut second_running_total = 0;

    for linechunk in &std::io::stdin().lines().chunks(3) {
        let mut badge_sets: Vec<HashSet<char>> = Vec::new();

        for line in linechunk {
            let line = line?;
            let line = line.trim();
            let n = line.len();

            assert!(n > 0);
            assert!(n % 2 == 0);

            let left: HashSet<char> = HashSet::from_iter(line[..(n / 2)].chars());
            let right: HashSet<char> = HashSet::from_iter(line[(n / 2)..].chars());
            let full: HashSet<char> = HashSet::from_iter(line.chars());

            badge_sets.push(full);

            let letter = left.intersection(&right).next().unwrap();
            let value = char_value(*letter);

            first_running_total += value;
        }

        let badge_set = &(&badge_sets[0] & &badge_sets[1]) & &badge_sets[2];
        let badge_letter = badge_set.iter().next().unwrap();

        second_running_total += char_value(*badge_letter);
    }

    println!("First question: {}", first_running_total);
    println!("Second question: {}", second_running_total);

    Ok(())
}
