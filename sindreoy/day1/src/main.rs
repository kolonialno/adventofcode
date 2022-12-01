use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    let elf_calories_vec: Vec<i32> = get_elf_calories();
    let max_calories = elf_calories_vec.iter().max();
    println!("{:?}", max_calories);

    Ok(())
}

fn get_elf_calories() -> Vec<i32> {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);

    // The total number of calories held by an elf
    let mut elf_calorie_vecs : Vec<i32> = Vec::new();
    let mut total_calories: i32 = 0;
    for line in reader.lines() {
        let line_content = line.unwrap();
        if line_content.len() > 0 {
            total_calories += line_content.parse::<i32>().unwrap()
        } else {
            elf_calorie_vecs.push(total_calories);
            println!("{}", total_calories);
            total_calories = 0;
        }
    }

    // Extract last entry if file didn't end in new line
    if total_calories > 0{
        elf_calorie_vecs.push(total_calories);
    }

    return elf_calorie_vecs;
}