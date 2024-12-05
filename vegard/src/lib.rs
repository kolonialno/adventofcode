use std::cmp::{max, min};
use std::fs::File;
use std::io::{self, BufRead};


pub fn day_0() {
    println!("Ho ho ho");
}
pub fn day_1() {
    let file = File::open("./data/1a.txt");
    let reader = io::BufReader::new(file.unwrap());

    let mut list_1: Vec<i32> = Vec::new();
    let mut list_2: Vec<i32> = Vec::new();

    for line in reader.lines() {
        let line_string = line.unwrap();
        let parts: Vec<&str> = line_string.split_whitespace().collect();
        list_1.push(parts[0].parse().unwrap());
        list_2.push(parts[1].parse().unwrap());
    }
    // let mut list_1 = vec![
    //     3,
    //     4,
    //     2,
    //     1,
    //     3,
    //     3,
    // ];
    // let mut list_2 = vec![
    //     4,
    //     3,
    //     5,
    //     3,
    //     9,
    //     3
    // ];
    list_1.sort();
    list_2.sort();

    let mut sum: i32 = 0;
    for (a,b) in list_1.iter().zip(list_2.iter()){
        sum += max(a,b)-min(a,b);
    };
    println!("Total deviation is {}", sum);

    let mut similarity_score: i32 = 0;
    for a in list_1.iter(){
        for b in list_2.iter(){
            if a == b {
                similarity_score += a;
            }
        }
    }
    println!("Similarity score is {}", similarity_score);
}

pub fn day_2() {
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