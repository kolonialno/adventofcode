use regex::Regex;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::str::Lines;

pub fn solve_a(program: &Vec<Op>) -> i32 {
    let mut machine = Machine::load(&program);
    machine.run();
    machine.acc
}

pub fn solve_b(_program: &Vec<Op>) -> i32 {
    0
}

#[derive(Clone, Copy, Debug)]
pub enum Op {
    Acc(i32),
    Jmp(i32),
    Nop(i32),
}

impl Op {
    pub fn from_lines(lines: Lines) -> Vec<Op> {
        let re = Regex::new(r"^(\w+) ([+-]\d+)$").unwrap();
        lines
            .filter_map(|l| {
                let caps = re.captures(l).expect("Instruction did not match regex");
                let arg = caps[2].parse().unwrap();
                match caps[1].as_ref() {
                    "acc" => Some(Op::Acc(arg)),
                    "jmp" => Some(Op::Jmp(arg)),
                    "nop" => Some(Op::Nop(arg)),
                    _ => None,
                }
            })
            .collect()
    }
}

#[derive(Debug)]
struct Machine {
    acc: i32,
    pc: i32,
    program: Vec<Op>,
    executed: HashMap<i32, usize>,
}

impl Machine {
    pub fn load(program: &Vec<Op>) -> Machine {
        Machine {
            acc: 0,
            pc: 0,
            program: program.clone(),
            executed: HashMap::new(),
        }
    }

    pub fn run(&mut self) {
        loop {
            match self.executed.entry(self.pc) {
                Entry::Vacant(entry) => {
                    entry.insert(1);
                }
                Entry::Occupied(_) => break, // Instruction has been executed before
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
        }
    }
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
}
