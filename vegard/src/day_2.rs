use std::fs::File;
use std::io::{self, BufRead};

fn solve() {
    let file = File::open("./data/2.txt");
    let reader = io::BufReader::new(file.unwrap());

    let mut num_safe_1 = 0;
    let mut num_safe_2 = 0;

    for line in reader.lines() {
        let line_string = line.unwrap();
        let parts: Vec<&str> = line_string.split_whitespace().collect();
        let report: Vec<i32> = parts.iter().map(|s| s.parse::<i32>().unwrap()).collect();

        let res = is_safe(&report);

        if res {
            num_safe_1 += 1;
            num_safe_2 += 1;
            continue;
        }

        // Try removing one element for rule 2
        for i in 0..report.len() {
            let mut new_report = report.clone();
            new_report.remove(i);
            let res = is_safe(&new_report);
            if res {
                num_safe_2 += 1;
                break;
            }
        }
    }

    println!("Num safe, rule 1: {}", num_safe_1);
    println!("Num safe, rule 2: {}", num_safe_2);

}

fn is_safe(report: &Vec<i32>) -> bool {
    let mut decreasing = true;
    let mut increasing = true;
    for i in 0..report.len()-1 {
        if report[i] >= report[i+1] {
            increasing = false;
        }
        if report[i] <= report[i+1] {
            decreasing = false;
        }
        if decreasing == false && increasing == false {
            return false;
        }
        if (report[i]-report[i+1]).abs() > 3 {
            return false;
        }
    }
    return true;
}

pub fn __main__() {
    solve();
}