use std::fs::File;
use std::io::{BufRead, BufReader, Error};

struct Command {
    forward: i64,
    down: i64,
    up: i64,
}
fn build_command(line: String) -> Command {
    let mut split = line.split(" ");
    let prefix = split.next().unwrap();
    let suffix: i64 = split.next().unwrap().parse().expect("Not a number!");

    match prefix {
        "forward" => Command {
            forward: suffix,
            down: 0,
            up: 0,
        },
        "down" => Command {
            forward: 0,
            down: suffix,
            up: 0,
        },
        "up" => Command {
            forward: 0,
            down: 0,
            up: suffix,
        },
        _ => panic!("crash and burn"),
    }
}

fn read_input(path: &str) -> Result<Vec<Command>, Error> {
    let file = File::open(path)?;
    let br = BufReader::new(file);
    let v: Vec<Command> = br
        .lines()
        .map(|l| build_command(l.expect("Could not parse line")))
        .collect();

    Ok(v)
}
fn problem_1(commands: &Vec<Command>) -> i64 {
    let mut horizontal_postion: i64 = 0;
    let mut depth: i64 = 0;

    for command in commands {
        depth += command.down - command.up;
        horizontal_postion += command.forward;
    }
    depth * horizontal_postion
}

fn problem_2(commands: &Vec<Command>) -> i64 {
    let mut horizontal_postion: i64 = 0;
    let mut depth: i64 = 0;
    let mut aim: i64 = 0;

    for command in commands {
        aim += command.down - command.up;
        horizontal_postion += command.forward;
        depth += aim * command.forward;
    }
    depth * horizontal_postion
}

fn main() -> Result<(), Error> {
    // Get input
    let test_commands = read_input("test_input.txt")?;
    let commands = read_input("input.txt")?;

    let test_solution_1 = problem_1(&test_commands);
    assert_eq!(test_solution_1, 150);

    let solution_1 = problem_1(&commands);
    println!("Solution problem 1: {}", solution_1);

    let test_solution_2 = problem_2(&test_commands);
    assert_eq!(test_solution_2, 900);

    let commands = read_input("input.txt")?;
    let solution_2 = problem_2(&commands);
    println!("Solution problem 2: {}", solution_2);

    Ok(())
}
