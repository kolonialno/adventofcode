use std::collections::VecDeque;

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

fn parse_instructions(input: &str) -> VecDeque<Instruction> {
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

struct CPU {
    x: isize,
    current_instruction: Option<Instruction>,
    remaining_cycles: usize,
    program: Program,
}

impl CPU {
    fn new(input: &str) -> Self {
        let program = parse_instructions(input);
        Self {
            x: 1,
            current_instruction: None,
            remaining_cycles: 0,
            program,
        }
    }
}

impl Iterator for CPU {
    type Item = isize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_cycles == 0 {
            if let Some(instruction) = &self.current_instruction {
                self.x += self.execute(instruction);
            }
            if let Some(instruction) = self.program.pop_front() {
                self.remaining_cycles = instruction.cycles();
                self.current_instruction = Some(instruction);
            } else {
                return None;
            }
        }

        self.remaining_cycles -= 1;

        Some(self.x)
    }
}

impl CPU {
    fn execute(&self, instruction: &Instruction) -> isize {
        match instruction {
            Instruction::Addx(value) => *value,
            Instruction::Noop => 0,
        }
    }
}

struct Display {
    pixels: [bool; 240],
}

impl Display {
    fn set(&mut self, i: usize, value: bool) {
        self.pixels[i] = value;
    }

    fn draw(&self) -> String {
        let mut result = String::new();
        for (i, pixel) in self.pixels.iter().enumerate() {
            if *pixel {
                result += "#";
            } else {
                result += ".";
            }
            if i > 0 && (i + 1) % 40 == 0 {
                result += "\n";
            }
        }

        result
    }
}

fn solve_part1(input: &str) -> isize {
    let cpu = CPU::new(input);

    let mut sum: isize = 0;
    for (i, value) in cpu.enumerate() {
        let instruction = (i as isize) + 1;
        if (instruction - 20) % 40 == 0 {
            sum = sum + (instruction * value);
        }
    }

    sum
}

fn solve_part2(input: &str) -> String {
    let cpu = CPU::new(input);
    let mut display = Display {
        pixels: [false; 240],
    };

    for (i, value) in cpu.enumerate() {
        let column = i % 40;
        let value = ((column as isize) - value).abs() <= 1;
        display.set(i, value);
    }

    display.draw()
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
    fn test_parse_instructions() {
        let input = "noop\naddx 3\naddx -5";
        let actual_instructions = parse_instructions(input);
        let expected_instructions = vec![
            Instruction::Noop,
            Instruction::Addx(3),
            Instruction::Addx(-5),
        ];
        for (actual, expected) in actual_instructions.iter().zip(expected_instructions) {
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
