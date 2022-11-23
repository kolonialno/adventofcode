use std::fs::File;
use std::io::{BufRead, BufReader, Error};

fn read_input(path: &str) -> Result<Vec<i64>, Error> {
    let file = File::open(path)?;
    let br = BufReader::new(file);
    let v: Vec<i64> = br
        .lines()
        .nth(0)
        .expect("Failed to parse line")
        .unwrap()
        .split(",")
        .map(|n| n.parse().unwrap())
        .collect();

    Ok(v)
}

fn solve(crabs: &Vec<i64>, f: &dyn Fn(i64, &i64) -> i64) -> i64 {
    let range = *crabs.iter().max().unwrap();
    let mut results = vec![0; range as usize];
    for i in 0..range {
        let result = crabs.iter().fold(0i64, |mut sum, n| {
            sum += f(i, n);
            sum
        });
        results[i as usize] = result;
    }
    *results.iter().min().unwrap()
}

fn problem_1(crabs: &Vec<i64>) -> i64 {
    fn f(p: i64, t: &i64) -> i64 {
        (p - t).abs()
    }
    solve(&crabs, &f)
}
fn problem_2(crabs: &Vec<i64>) -> i64 {
    fn f(p: i64, t: &i64) -> i64 {
        let n = (p - t).abs();
        (n * (n + 1)) / 2
    }

    solve(&crabs, &f)
}

fn main() -> Result<(), Error> {
    let test_crabs = read_input("test_input.txt")?;
    let crabs = read_input("input.txt")?;

    let test_solution_1 = problem_1(&test_crabs);
    assert_eq!(test_solution_1, 37);

    let solution_1 = problem_1(&crabs);
    println!("Solution problem 1: {}", solution_1);

    let test_solution_2 = problem_2(&test_crabs);
    assert_eq!(test_solution_2, 168);

    let crabs = read_input("input.txt")?;
    let solution_2 = problem_2(&crabs);
    println!("Solution problem 2: {}", solution_2);

    Ok(())
}
