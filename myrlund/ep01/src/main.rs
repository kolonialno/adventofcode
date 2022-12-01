use std::{
    collections::HashSet,
    io::{self, BufRead},
};

struct TopCaloriedElves {
    num_tracked_elves: usize,
    top_elves: HashSet<i32>,
    last_top_elf: Option<i32>,
}

impl TopCaloriedElves {
    fn new(num_tracked_elves: usize) -> Self {
        Self {
            num_tracked_elves,
            top_elves: HashSet::new(),
            last_top_elf: None,
        }
    }

    fn maybe_add_top_elf(&mut self, elf_calories: i32) {
        let should_add = if let Some(last_top_elf) = self.last_top_elf {
            last_top_elf < elf_calories
        } else {
            true
        };

        if should_add {
            self.add_top_elf(elf_calories);
        }
    }

    fn add_top_elf(&mut self, elf_calories: i32) {
        if self.top_elves.len() == self.num_tracked_elves {
            let last_top_elf = self.last_top_elf.expect("last top elf should be tracked");
            self.top_elves.remove(&last_top_elf);
        }

        self.top_elves.insert(elf_calories);
        self.last_top_elf = self.top_elves.iter().min().copied();
    }

    fn sum_calories(&self) -> i32 {
        self.top_elves.iter().sum()
    }
}

fn run(reader: impl BufRead, num_elves: usize) -> i32 {
    let mut top_elves = TopCaloriedElves::new(num_elves);

    let mut current_elf_calories = 0;
    for line in reader.lines().map(|l| l.unwrap()) {
        if !line.is_empty() {
            current_elf_calories += line.parse::<i32>().unwrap();
            continue;
        }

        top_elves.maybe_add_top_elf(current_elf_calories);

        current_elf_calories = 0;
    }

    top_elves.maybe_add_top_elf(current_elf_calories);

    top_elves.sum_calories()
}

fn main() {
    let total_calories = run(io::stdin().lock(), 3);

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

        assert_eq!(run(buf, 3), 45000);
    }
}
