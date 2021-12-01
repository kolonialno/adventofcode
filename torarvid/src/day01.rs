use crate::util::file_by_lines_as_i32;

pub fn run() {
    let numbers = file_by_lines_as_i32("day01.txt");
    let mut count = 0;
    for i in 1..numbers.len() {
        if numbers[i] > numbers[i - 1] {
            count += 1;
        }
    }
    println!("Part 1: {}", count);

    // Part 2
    let mut group_sum: Option<i32> = None;
    count = 0;
    for i in 2..numbers.len() {
        let new_group_sum = numbers[i - 2] + numbers[i - 1] + numbers[i];
        if group_sum.is_some() && group_sum.unwrap() < new_group_sum {
            count += 1;
        }
        group_sum = Some(new_group_sum);
    }
    println!("Part 2: {}", count);
}

