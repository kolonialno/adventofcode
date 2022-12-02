use std::error::Error;
use std::fs;

fn main() -> Result<(), Box<dyn Error>> {
    let mut elf_cals: Vec<u32> = fs::read_to_string("input.txt")?
        .split("\n\n")
        .map(|cals_string| {
            cals_string
                .split("\n")
                .map(|cals| cals.parse::<u32>().unwrap())
                .sum()
        })
        .collect();
    elf_cals.sort();

    let answer1 = elf_cals.last().unwrap();
    println!("{}", answer1);

    let answer2 = elf_cals.iter().rev().take(3).sum::<u32>();
    println!("{}", answer2);

    Result::Ok(())
}
