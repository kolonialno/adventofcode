use std::env;
use std::fs;
use std::io::{self, BufRead};
use std::path;

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    let n = &args[2]
        .parse::<usize>()
        .expect("Need to be a numerical value");

    let mut calorie_count = 0;
    let mut calorie_vec = vec![];

    if let Ok(lines) = read_lines(file_path) {
        for line in lines {
            if let Ok(line) = line {
                let l = line.parse::<u32>();
                match l {
                    Ok(calorie) => calorie_count += calorie,
                    Err(_) => {
                        calorie_vec.push(calorie_count);
                        calorie_count = 0;
                    }
                }
            }
        }
    }

    calorie_vec.sort();

    let sum_calories: u32 = calorie_vec.iter().rev().take(*n).sum();

    println!("Max calories: {}", sum_calories);
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<fs::File>>>
where
    P: AsRef<path::Path>,
{
    let file = fs::File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
