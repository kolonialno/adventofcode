use std::fs;

fn main() {
    // Read the input into a string, panicing if reading fails
    let file = fs::read_to_string("./input.txt").expect("Failed to read file");

    // Parse the file into a list of lists of numbers
    let numbers: Vec<Vec<i32>> = file.split("\n\n").map(|x| {
        x.split("\n").filter(|num| !num.is_empty()).map(|num| num.parse::<i32>().unwrap()).collect()
    }).collect();

    // Summarize the numbers per elf
    let sums_by_elf: Vec<i32> = numbers.iter().map(|numbers| numbers.iter().sum()).collect();

    // Find the higest number
    let elf_with_max = sums_by_elf.iter().reduce(|highest, current| {
        if highest > current { highest } else { current }
    }).expect("No elfs?");

    println!("Elf with higest sum: {elf_with_max:?}");

    // Sort the sums
    let mut sorted_sums: Vec<i32> = sums_by_elf.clone();
    sorted_sums.sort_by(|a, b| b.cmp(a));

    // How the hell does one take the x first elements of a list in rust? Who knows 🤷
    let top_three_sum = sorted_sums[0] + sorted_sums[1] + sorted_sums[2];

    println!("Sum of top three elfs: {top_three_sum:?}");
}
