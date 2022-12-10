use std::str::FromStr;

#[derive(Debug)]
enum Instruction {
    Noop,
    AddX(isize),
}

impl Instruction {
    fn cycles(&self) -> usize {
        match self {
            Self::Noop => 1,
            Self::AddX(_) => 2,
        }
    }
}

impl FromStr for Instruction {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("addx") {
            let value = s[5..].parse()?;
            Ok(Self::AddX(value))
        } else {
            Ok(Self::Noop)
        }
    }
}

struct Scheduler {
    clock: usize,
    register: isize,

    signal_strength_samples: Vec<isize>,
    // display: Vec<Vec<char>>,
    display: String,
}

impl Scheduler {
    fn process_instruction(&mut self, instruction: &Instruction) {
        // For each tick of the number of cycles, run the tick routine.
        for _ in 0..instruction.cycles() {
            self.tick();
        }

        // Finish processing, then advance the clock.
        match instruction {
            Instruction::AddX(val) => {
                self.register += val;
            }
            Instruction::Noop => (),
        }
    }

    fn tick(&mut self) {
        // Write the pixel at the current position
        self.write_pixel();

        // Advance the clock/drawing position
        self.clock += 1;

        // Consider whether we should be sampling at the current clock cycle, before finishing the processing of the
        let should_sample = (20 - self.clock as isize).rem_euclid(40) == 0;
        if should_sample {
            self.sample_signal_strength(self.clock);
        }
    }

    fn write_pixel(&mut self) {
        let screen_width = 40;
        let x_position = self.clock % screen_width;

        // Write the pixel
        let is_pixel_lit = (self.register - 1..=self.register + 1).contains(&(x_position as isize));
        self.display.push(if is_pixel_lit { '#' } else { '.' });

        // Write line breaks for formatting
        if x_position + 1 == screen_width {
            self.display.push('\n');
        }
    }

    fn sample_signal_strength(&mut self, clock_cycle: usize) {
        let signal_strength = self.register * (clock_cycle as isize);
        self.signal_strength_samples.push(signal_strength);
    }

    fn read_display(&self) -> String {
        self.display.clone()
    }
}

fn execute_instructions(s: &str) -> Scheduler {
    let mut scheduler = Scheduler {
        clock: 0,
        register: 1,
        signal_strength_samples: vec![],
        display: String::new(),
    };

    let instructions: Vec<Instruction> = s.lines().map(|line| line.parse().unwrap()).collect();
    for instruction in instructions {
        scheduler.process_instruction(&instruction);
    }

    scheduler
}

fn run_part_one(s: &str) -> isize {
    let scheduler = execute_instructions(s);
    scheduler.signal_strength_samples.iter().sum()
}

fn run_part_two(s: &str) -> String {
    let scheduler = execute_instructions(s);
    scheduler.read_display()
}

fn main() {
    let input = include_str!("../input.txt");

    let part_one = run_part_one(input);
    println!("Part 1: {part_one}");

    let part_two = run_part_two(input);
    println!("Part 2: \n{part_two}");
}

#[cfg(test)]
mod tests {
    use crate::{run_part_one, run_part_two};

    static SAMPLE: &str = include_str!("../sample.txt");

    #[test]
    fn sample_part_one_is_correct() {
        assert_eq!(run_part_one(SAMPLE), 13140);
    }

    #[test]
    fn sample_after_round_20() {
        assert_eq!(
            run_part_one(
                "addx 1
addx 1
addx 1
addx 1
addx 1
addx 1
addx 1
addx 1
addx 1
addx 1
"
            ),
            (1 + 9) * 20
        );
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(
            run_part_two(SAMPLE),
            "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."
        );
    }
}
