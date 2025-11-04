use std::vec;

use crate::utils::read_to_int_vectors;

fn solve() {
    // Get rules and prints
    let rules: Vec<Vec<i32>> = read_to_int_vectors("data/5_rules.txt", "|");
    let prints: Vec<Vec<i32>> = read_to_int_vectors("data/5.txt", ",");

    // Part 1
    let mut valid_prints: Vec<Vec<i32>> = vec![];
    let mut invalid_prints: Vec<Vec<i32>> = vec![];
    for print in prints {
        let mut is_valid = true;
        for (i, val) in print.iter().enumerate() {
            let invalid_predecessors = rules
                .iter()
                .filter(|rule| rule[0] == *val)
                .map(|rule| rule[1])
                .collect::<Vec<i32>>();
            for pred in print[0..i].iter() {
                if invalid_predecessors.contains(pred) {
                    is_valid = false;
                    break;
                }
            }
            if !is_valid {
                break;
            }
        }
        if is_valid {
            valid_prints.push(print);
        } else {
            invalid_prints.push(print);
        }
    }

    let result: i32 = valid_prints
        .iter()
        .map(|print| print[print.len() / 2 as usize])
        .sum();
    println!("Result task 1: {}", result);

    // Part 2
    let res: i32 = invalid_prints
        .iter()
        .map(|print| {
            print
                .iter()
                .find(|&page| is_middle_print(page, print, &rules))
                .unwrap()
        })
        .sum();
    println!("Result task 2: {}", res);
}

fn is_middle_print(page: &i32, print: &Vec<i32>, rules: &Vec<Vec<i32>>) -> bool {
    let middle_offset: i32 = print
        .iter()
        .filter(|&compare_page| compare_page != page)
        .map(|compare_page| {
            if rules
                .iter()
                .find(|&rule| rule[0] == *compare_page && rule[1] == *page)
                .is_some()
            {
                1
            } else {
                // No need to search here, must be before or after
                -1
            }
        })
        .sum();

    return middle_offset == 0;
}

pub fn __main__() {
    solve();
}
