fn main() {
    let input = include_str!("../input.txt");
    let mut elves = parse(input);
    println!("{} elves", elves.len());

    sort_elves(&mut elves);
    
    match find_max(&mut elves) {
        Some(elf) => println!("Max calories: {}", elf.total_calories()),
        None => eprintln!("No elves, and no max")
    }
    
    let top_3 = top_n_calories(elves, 3);
    println!("Top 3 elf calories: {top_3}");
}

struct Elf {
    snacks: Vec<i64>,
}

impl Elf {
    fn total_calories(&self) -> i64 {
        self.snacks.iter().sum()
    }
}

fn parse(input: &str) -> Vec<Elf> {
    input
        .split("\n\n")
        .map(|calories| {
            let snacks = calories.split('\n').filter_map(|snack| {
                snack.parse::<i64>().ok()
            }).collect();
            Elf { snacks }
        })
        .collect()
}

fn sort_elves(elves: &mut [Elf]) {
    elves.sort_by(|e1, e2| {
        let ec1 = e1.total_calories();
        let ec2 = e2.total_calories();
        ec1.cmp(&ec2)
    });
    elves.reverse();
}

fn find_max(elves: &mut [Elf]) -> Option<&Elf> {
    elves.first()
}

// Assumes previous sort
fn top_n_calories(elves: Vec<Elf>, n: usize) -> i64 {
    elves.iter().take(n).map(|e| e.total_calories()).sum()
}

#[cfg(test)]
mod test {
    use crate::{parse, Elf, find_max, sort_elves, top_n_calories};

    #[test]
    fn parse_to_elf() {
        let input = include_str!("../sample.txt");
        let elves: Vec<Elf> = parse(input);
        assert_eq!(elves.len(), 5);
    }

    #[test]
    fn parse_calories() {
        let input = include_str!("../sample.txt");
        let elves: Vec<Elf> = parse(input);
        let elf = &elves[0];
        assert_eq!(elf.snacks.len(), 3);
    }
    
    #[test]
    fn total_calories() {
        let input = include_str!("../sample.txt");
        let elves: Vec<Elf> = parse(input);
        let elf = &elves[0];
        assert_eq!(elf.total_calories(), 6000);
    }
    
    #[test]
    fn max_calories() {
        let input = include_str!("../sample.txt");
        let mut elves: Vec<Elf> = parse(input);
        sort_elves(&mut elves);
        let max = find_max(&mut elves).unwrap();
        assert_eq!(max.total_calories(), 24000);
    }
    
    #[test]
    fn top_3_calories() {
        let input = include_str!("../sample.txt");
        let mut elves: Vec<Elf> = parse(input);
        sort_elves(&mut elves);
        assert_eq!(top_n_calories(elves, 3), 45000);
    }
}
