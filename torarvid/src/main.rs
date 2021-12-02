use std::env;
mod util;
mod day01;
mod day02;

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
        _ => println!("Day {} not implemented yet", day),
    }
}

fn run(day: i32, f: fn()) {
    println!("Running day {}", day);
    f();
}
