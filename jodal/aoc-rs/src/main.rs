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
    }
}
