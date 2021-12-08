use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

fn read<R: Read>(io: R) -> Result<Vec<usize>, Error> {
    Ok(BufReader::new(io)
        .lines()
        .next()
        .unwrap()?
        .trim()
        .split(',')
        .map(|i| i.parse().unwrap())
        .collect())
}

pub fn day06() {
    let inital_ages = read(File::open("inputs/day06.txt").unwrap()).unwrap();
    let mut fish_by_age = vec![0_u64; 9];

    for age in inital_ages.iter() {
        fish_by_age[*age] += 1;
    }

    for day in 1..257 {
        let new_8 = fish_by_age[0];
        fish_by_age[0] = 0;
        for age in 1..9 {
            fish_by_age[(age + 8) % 9] = fish_by_age[age];
        }
        fish_by_age[6] += new_8;
        fish_by_age[8] = new_8;

    }
    println!("total: {}, fishes: {:?}" ,fish_by_age.iter().sum::<u64>(), fish_by_age);

}
