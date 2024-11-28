use std::{collections::VecDeque, ops::Index, ops::IndexMut};

#[derive(Debug, PartialEq, Eq)]
enum Instruction {
    Addx(isize),
    Noop,
}

impl Instruction {
    fn cycles(&self) -> usize {
        match &self {
            Self::Addx(_) => 2,
            Self::Noop => 1,
        }
    }
}

type Program = VecDeque<Instruction>;

fn parse_program(input: &str) -> Program {
    input
        .lines()
        .map(|line| {
            if line == "noop" {
                return Instruction::Noop;
            }

            let (instruction, arg) = line.split_once(" ").expect("Invalid instruction");

            match instruction {
                "addx" => Instruction::Addx(
                    arg.parse::<isize>()
                        .expect("Invalid argument to addx instruction"),
                ),
                _ => panic!("Invalid instruction"),
            }
        })
        .collect()
}

struct Registers {
    x: isize,
}

struct CPU {
    /// The program to execute, in the form of a sequence of instruction
    program: Program,
    /// Current state of registers
    registers: Registers,
    /// The instruction that's currently being executed
    current_instruction: Option<Instruction>,
    /// Remaining cycles of the current instruction
    remaining_cycles: usize,
}

impl CPU {
    fn new(input: &str) -> Self {
        let program = parse_program(input);
        Self {
            program,
            registers: Registers { x: 1 },
            current_instruction: None,
            remaining_cycles: 0,
        }
    }

    // Execute the current instruction
    fn execute(&mut self) {
        if let Some(instruction) = &self.current_instruction {
            match instruction {
                Instruction::Addx(value) => self.registers.x += value,
                Instruction::Noop => (),
            }
            // Reset state to ensure we don't re-execute the same instruction
            self.current_instruction = None;
            self.remaining_cycles = 0;
        }
    }

    /// Load the next instruction, if any left in the current program
    fn load_next(&mut self) -> bool {
        if let Some(instruction) = self.program.pop_front() {
            self.remaining_cycles = instruction.cycles();
            self.current_instruction = Some(instruction);
            return true;
        }

        false
    }

    fn tick(&mut self) -> bool {
        // The previous instruction takes affect _after_ the cycle,
        // so we update it only when the next cycle starts
        if self.remaining_cycles == 0 {
            self.execute();
        }
        if self.current_instruction.is_none() {
            if !self.load_next() {
                return false;
            }
        }

        self.remaining_cycles -= 1;

        return true;
    }
}

struct Display {
    pixels: [bool; 240],
}

impl Display {
    fn draw(&self) -> String {
        let mut result = String::new();
        for (i, pixel) in self.pixels.iter().enumerate() {
            if *pixel {
                result += "█";
            } else {
                result += " ";
            }
            if i > 0 && (i + 1) % 40 == 0 {
                result += "\n";
            }
        }

        result
    }
}

impl Index<usize> for Display {
    type Output = bool;

    fn index(&self, index: usize) -> &Self::Output {
        &self.pixels[index]
    }
}

impl IndexMut<usize> for Display {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.pixels[index]
    }
}

struct Device {
    cpu: CPU,
    display: Display,
    cycle: usize,
}

impl Device {
    fn new(input: &str) -> Self {
        let cpu = CPU::new(input);
        let display = Display {
            pixels: [false; 240],
        };

        Device {
            cpu,
            display,
            cycle: 0,
        }
    }

    /// Run until the CPU has executed the entire program
    fn run(&mut self) {
        while self.cpu.tick() {
            // Update the display based on the x registry of the CPU
            let column = self.cycle % 40;
            let num_pixels = self.display.pixels.len();
            let value = ((column as isize) - self.cpu.registers.x).abs() <= 1;
            self.display[self.cycle % num_pixels] = value;
            // Update cycle counter as we're now done with the cycle
            // (zero indexing makes the calculation above simpler)
            self.cycle += 1;
        }
    }
}

fn solve_part1(input: &str) -> isize {
    let mut cpu = CPU::new(input);

    let mut sum: isize = 0;
    let mut cycle: isize = 0;
    while cpu.tick() {
        cycle += 1;
        if (cycle as isize - 20) % 40 == 0 {
            sum = sum + (cycle as isize * cpu.registers.x);
        }
    }

    sum
}

fn solve_part2(input: &str) -> String {
    let mut device = Device::new(input);
    device.run();
    device.display.draw()
}

fn main() {
    let input = include_str!("./input.txt");
    let sum = solve_part1(input);
    println!("Part 1: {sum}");
    let sum = solve_part2(input);
    println!("Part 2: \n{sum}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_program() {
        let input = "noop\naddx 3\naddx -5";
        let actual_program = parse_program(input);
        let expected_program = vec![
            Instruction::Noop,
            Instruction::Addx(3),
            Instruction::Addx(-5),
        ];
        for (actual, expected) in actual_program.iter().zip(expected_program) {
            assert_eq!(*actual, expected);
        }
    }

    #[test]
    fn test_part1() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part1(input), 13140, "Wrong result for pt. 1");
    }

    #[test]
    fn test_part2() {
        let input = include_str!("./test.txt");
        let output: String = include_str!("./test_output.txt").into();
        assert_eq!(solve_part2(input), output, "Wrong result for pt. 2");
    }
}
