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
}

fn main() {
    let _opt = Opt::from_args();
    let command = Command::from_args();

    match command {
        Command::Day01a => {
            if let Ok(numbers) = aoc::read_numbers("input/day1a.txt") {
                println!("Answer: {}", aoc::day01a::solve(&numbers).unwrap());
            }
        }
    }
}
