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
    /// Day 2: Password Philosophy, part two
    Day02b,
    /// Day 3: Toboggan Trajectory
    Day03a,
}

fn main() {
    let _opt = Opt::from_args();
    let command = Command::from_args();

    match command {
        Command::Day01a => {
            if let Ok(numbers) = aoc::read_numbers("input/day01.txt") {
                println!("Answer: {}", aoc::day01::solve_a(&numbers).unwrap());
            }
        }
        Command::Day01b => {
            if let Ok(numbers) = aoc::read_numbers("input/day01.txt") {
                println!("Answer: {}", aoc::day01::solve_b(&numbers).unwrap());
            }
        }
        Command::Day02a => {
            let lines = aoc::read_lines("input/day02.txt").expect("Failed reading file");
            let entries = aoc::day02::parse(lines);
            let answer = aoc::day02::solve_a(entries);
            println!("Answer: {}", answer);
        }
        Command::Day02b => {
            let lines = aoc::read_lines("input/day02.txt").expect("Failed reading file");
            let entries = aoc::day02::parse(lines);
            let answer = aoc::day02::solve_b(entries);
            println!("Answer: {}", answer);
        }
        Command::Day03a => {
            let lines = aoc::read_lines("input/day03.txt").expect("Failed reading file");
            let map = aoc::day03::Map::from_lines(lines);
            let answer = aoc::day03::solve_a(map);
            println!("Answer: {}", answer);
        }
    }
}
