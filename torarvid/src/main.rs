use std::env;
mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod util;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Please provide a day number");
        return;
    }
    let day = args[1].parse::<i32>().unwrap();
    match day {
        1 => run(day, day01::run),
        2 => run(day, day02::run),
        3 => run(day, day03::run),
        4 => run(day, day04::run),
        5 => run(day, day05::run),
        6 => run(day, day06::run),
        7 => run(day, day07::run),
        8 => run(day, day08::run),
        9 => run(day, day09::run),
        10 => run(day, day10::run),
        11 => run(day, day11::run),
        12 => run(day, day12::run),
        13 => run(day, day13::run),
        _ => println!("Day {} not implemented yet", day),
    }
}

fn run(day: i32, f: fn()) {
    println!("Running day {}", day);
    f();
}
