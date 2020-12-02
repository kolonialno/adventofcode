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
    Day01a,
    /// Day 1: Report Repair, part two
    Day01b,
    /// Day 2: Password Philosophy
    Day02a,
}

fn main() {
    let _opt = Opt::from_args();
    let command = Command::from_args();

    match command {
        Command::Day01a => {
            if let Ok(numbers) = aoc::read_numbers("input/day01.txt") {
                println!("Answer: {}", aoc::day01a::solve(&numbers).unwrap());
            }
        }
        Command::Day01b => {
            if let Ok(numbers) = aoc::read_numbers("input/day01.txt") {
                println!("Answer: {}", aoc::day01b::solve(&numbers).unwrap());
            }
        }
        Command::Day02a => {
            let lines = aoc::read_lines("input/day02.txt").expect("Failed reading file");
            let entries = aoc::day02a::parse(lines);
            let answer = aoc::day02a::solve(entries);
            println!("Answer: {}", answer);
        }
    }
}
