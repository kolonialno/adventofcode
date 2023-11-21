use std::io::{self, BufRead};

struct ElfCalorieTracker {
    top_elves: [usize; 3],
}

impl ElfCalorieTracker {
    fn new() -> Self {
        Self {
            top_elves: [0, 0, 0],
        }
    }

    pub fn track_elf(&mut self, candidate: usize) {
        let least = self.top_elves[0];

        if candidate > least {
            self.top_elves[0] = candidate;
            self.top_elves.sort();
        }
    }

    pub fn sum_calories(&self) -> usize {
        self.top_elves.iter().sum()
    }
}

fn run(reader: impl BufRead) -> usize {
    let mut top_elves = ElfCalorieTracker::new();

    let mut current_elf_calories = 0;
    for line in reader.lines().map(|l| l.unwrap()) {
        if !line.is_empty() {
            current_elf_calories += line.parse::<usize>().unwrap();
            continue;
        }

        top_elves.track_elf(current_elf_calories);

        current_elf_calories = 0;
    }

    top_elves.track_elf(current_elf_calories);

    top_elves.sum_calories()
}

fn main() {
    let total_calories = run(io::stdin().lock());

    println!("{total_calories}");
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::BufReader};

    use crate::run;

    #[test]
    fn sample_is_correct() {
        let f = File::open("sample.txt").unwrap();
        let buf = BufReader::new(f);

        assert_eq!(run(buf), 45000);
    }
}
