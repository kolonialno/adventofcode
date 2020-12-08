use regex::Regex;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::str::Lines;

pub fn solve_a(program: &Vec<Op>) -> i32 {
    let mut machine = Machine::load(program.clone());
    let exit_status = machine.run();
    assert_eq!(exit_status, ExitStatus::InfiniteLoop);
    machine.acc
}

pub fn solve_b(program: &Vec<Op>) -> i32 {
    for (index, &op) in program.iter().enumerate() {
        let mut patched_program = program.clone();
        patched_program[index] = match op {
            Op::Acc(_) => op,
            Op::Jmp(arg) => Op::Nop(arg),
            Op::Nop(arg) => Op::Jmp(arg),
        };
        let mut machine = Machine::load(patched_program);
        match machine.run() {
            ExitStatus::Success => return machine.acc,
            ExitStatus::InfiniteLoop => {}
        };
    }
    -1
}

#[derive(Clone, Copy, Debug)]
pub enum Op {
    Acc(i32),
    Jmp(isize),
    Nop(isize),
}

impl Op {
    pub fn from_lines(lines: Lines) -> Vec<Op> {
        let re = Regex::new(r"^(\w+) ([+-]\d+)$").unwrap();
        lines
            .filter_map(|l| {
                let caps = re.captures(l).expect("Instruction did not match regex");
                match caps[1].as_ref() {
                    "acc" => Some(Op::Acc(caps[2].parse().unwrap())),
                    "jmp" => Some(Op::Jmp(caps[2].parse().unwrap())),
                    "nop" => Some(Op::Nop(caps[2].parse().unwrap())),
                    _ => None,
                }
            })
            .collect()
    }
}

#[derive(Debug)]
struct Machine {
    acc: i32,
    pc: isize,
    program: Vec<Op>,
    executed: HashMap<isize, usize>,
}

impl Machine {
    pub fn load(program: Vec<Op>) -> Machine {
        Machine {
            acc: 0,
            pc: 0,
            program: program,
            executed: HashMap::new(),
        }
    }

    pub fn run(&mut self) -> ExitStatus {
        loop {
            match self.executed.entry(self.pc) {
                Entry::Vacant(entry) => {
                    entry.insert(1);
                }
                Entry::Occupied(_) => return ExitStatus::InfiniteLoop,
            }
            match self.program.get(self.pc as usize) {
                Some(&op) => {
                    // println!("pc={:?} acc={:?} op={:?}", self.pc, self.acc, op);
                    match op {
                        Op::Acc(arg) => {
                            self.acc += arg;
                            self.pc += 1;
                        }
                        Op::Jmp(arg) => {
                            self.pc += arg;
                        }
                        Op::Nop(_) => {
                            self.pc += 1;
                        }
                    }
                }
                None => panic!("Instruction not found"),
            }
            if self.pc == self.program.len() as isize {
                return ExitStatus::Success;
            }
        }
    }
}

#[derive(PartialEq, Debug)]
enum ExitStatus {
    Success,
    InfiniteLoop,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_a() {
        let input = String::from(
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
        let program = Op::from_lines(input.lines());
        assert_eq!(solve_a(&program), 5);
    }

    #[test]
    fn example_b() {
        let input = String::from(
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
        let program = Op::from_lines(input.lines());
        assert_eq!(solve_b(&program), 8);
    }
}
