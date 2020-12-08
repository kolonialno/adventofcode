use std::fs::read_to_string;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "aoc", about = "Solve Advent of Code puzzles.")]
struct Opt {
    #[structopt(subcommand)]
    cmd: Command,
}

#[derive(Debug, StructOpt)]
enum Command {
    /// Day 1: Report Repair
    Day01,
    /// Day 2: Password Philosophy
    Day02,
    /// Day 3: Toboggan Trajectory
    Day03,
    /// Day 4: Passport Processing
    Day04,
    /// Day 5: Binary Boarding
    Day05,
    /// Day 6: Custom Customs
    Day06,
    /// Day 7: Handy Haversacks
    Day07,
    /// Day 8: Handheld Halting
    Day08,
}

fn main() -> Result<(), std::io::Error> {
    let _opt = Opt::from_args();
    let command = Command::from_args();

    match command {
        Command::Day01 => {
            let input = read_to_string("input/day01.txt")?;
            let numbers = aoc::day01::parse_numbers(input.lines());
            println!("Part one: {}", aoc::day01::solve_a(&numbers).unwrap());
            println!("Part two: {}", aoc::day01::solve_b(&numbers).unwrap());
        }
        Command::Day02 => {
            let input = read_to_string("input/day02.txt")?;
            let entries = aoc::day02::PasswordEntry::from_lines(input.lines());
            println!("Part one: {}", aoc::day02::solve_a(&entries));
            println!("Part two: {}", aoc::day02::solve_b(&entries));
        }
        Command::Day03 => {
            let input = read_to_string("input/day03.txt")?;
            let map = aoc::day03::Map::from_lines(input.lines());
            println!("Part one: {}", aoc::day03::solve_a(&map));
            println!("Part two: {}", aoc::day03::solve_b(&map));
        }
        Command::Day04 => {
            let input = read_to_string("input/day04.txt")?;
            let passports = aoc::day04::Passport::from_lines(input.lines());
            println!("Part one: {}", aoc::day04::solve_a(&passports));
            println!("Part two: {}", aoc::day04::solve_b(&passports));
        }
        Command::Day05 => {
            let input = read_to_string("input/day05.txt")?;
            let boarding_passes = aoc::day05::BoardingPass::from_lines(input.lines());
            println!("Part one: {}", aoc::day05::solve_a(&boarding_passes));
            println!("Part two: {}", aoc::day05::solve_b(&boarding_passes));
        }
        Command::Day06 => {
            let input = read_to_string("input/day06.txt")?;
            let group_answers = aoc::day06::GroupAnswer::from_lines(input.lines());
            println!("Part one: {}", aoc::day06::solve_a(&group_answers));
            println!("Part two: {}", aoc::day06::solve_b(&group_answers));
        }
        Command::Day07 => {
            let input = read_to_string("input/day07.txt")?;
            let bag_rules = aoc::day07::BagRules::from_lines(input.lines());
            println!("Part one: {}", aoc::day07::solve_a(&bag_rules));
            println!("Part two: {}", aoc::day07::solve_b(&bag_rules));
        }
        Command::Day08 => {
            let input = read_to_string("input/day08.txt")?;
            let program = aoc::day08::Op::from_lines(input.lines());
            println!("Part one: {}", aoc::day08::solve_a(&program));
            println!("Part two: {}", aoc::day08::solve_b(&program));
        }
    }

    Ok(())
}
