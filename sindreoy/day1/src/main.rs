use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    let mut elf_calories_vec: Vec<i32> = get_elf_calories();

    elf_calories_vec.sort();

    let length = elf_calories_vec.len();

    let top1 = elf_calories_vec[length-1];
    let top2 = elf_calories_vec[length-2];
    let top3 = elf_calories_vec[length-3];
    let sum_top3 = top1 + top2 + top3;

    println!("{}", top1);
    println!("{}", top2);
    println!("{}", top3);
    println!("{}", sum_top3);

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
            total_calories = 0;
        }
    }

    // Extract last entry if file didn't end in new line
    if total_calories > 0{
        elf_calorie_vecs.push(total_calories);
    }

    return elf_calorie_vecs;
}