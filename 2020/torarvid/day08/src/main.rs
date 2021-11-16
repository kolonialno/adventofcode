use std::collections::HashSet;
use std::format;
use std::fs;
use std::str::Lines;

fn main() {
    let filename = "input.txt";
    let contents =
        fs::read_to_string(filename).expect(&format!("Failed to read file {}", filename));
    println!("Part1: {}", solve_part1(contents.lines()));
    println!("Part2: {}", solve_part2(contents.lines()));
}

#[derive(Debug, PartialEq, Clone)]
enum Command {
    Acc(i32),
    Jmp(i32),
    Nop(i32),
}

impl Command {
    fn parse(input: &str) -> Command {
        let parts: Vec<&str> = input.split(" ").collect();
        let command = match parts[0] {
            "acc" => Command::Acc(parts[1].parse::<i32>().unwrap()),
            "nop" => Command::Nop(parts[1].parse::<i32>().unwrap()),
            "jmp" => Command::Jmp(parts[1].parse::<i32>().unwrap()),
            _ => panic!("Unknown command"),
        };
        command
    }
}

#[derive(Debug)]
struct Program {
    debug: bool,
    acc: i32,
    commands: Vec<Command>,
    ip: usize,
}

enum ExitReason {
    InfiniteLoop,
    RanToCompletion,
}

impl Program {
    fn create(input: Lines) -> Program {
        let commands: Vec<Command> = input.map(Command::parse).collect();
        Program {
            debug: false,
            acc: 0,
            commands,
            ip: 0,
        }
    }

    fn step(&mut self) {
        if self.debug {
            println!("STEP {} {} {:?}", self.ip, self.acc, self.commands[self.ip]);
        }
        match self.commands[self.ip] {
            Command::Nop(_) => self.ip += 1,
            Command::Acc(delta) => {
                self.ip += 1;
                self.acc += delta;
            }
            Command::Jmp(delta) => self.ip = (self.ip as i32 + delta) as usize,
        }
    }

    fn run(&mut self) -> ExitReason {
        let mut seen_ip_locations = HashSet::new();
        loop {
            let not_seen_yet = seen_ip_locations.insert(self.ip);
            if not_seen_yet {
                if self.ip >= self.commands.len() {
                    return ExitReason::RanToCompletion;
                }
                self.step();
            } else {
                return ExitReason::InfiniteLoop;
            }
        }
    }
}

fn solve_part1(input: Lines) -> i32 {
    let mut program = Program::create(input);
    match program.run() {
        ExitReason::InfiniteLoop => return program.acc,
        _ => panic!("WTF?!"),
    }
}

fn solve_part2(input: Lines) -> i32 {
    let program = Program::create(input);
    for (ip, cmd) in program.commands.iter().enumerate() {
        let maybe_changed_command = match cmd {
            Command::Nop(i) => Some(Command::Jmp(*i)),
            Command::Jmp(i) => Some(Command::Nop(*i)),
            _ => None,
        };
        if let Some(changed_command) = maybe_changed_command {
            let mut changed_program = Program {
                debug: program.debug,
                commands: program.commands.clone(),
                acc: 0,
                ip: 0,
            };
            changed_program.commands[ip] = changed_command;
            if let ExitReason::RanToCompletion = changed_program.run() {
                return changed_program.acc;
            }
        }
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let test_data = String::from(
            "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6",
        );
        assert_eq!(solve_part1(test_data.lines()), 5);
        assert_eq!(solve_part2(test_data.lines()), 8);
    }
}
