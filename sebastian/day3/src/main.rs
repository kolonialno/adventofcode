use std::collections::HashSet;

struct Backpack {
    compartment1: HashSet<char>,
    compartment2: HashSet<char>,
}

impl From<&str> for Backpack {
    fn from(items: &str) -> Self {
        Backpack {
            compartment1: HashSet::from_iter(items[0..items.len() / 2].chars()),
            compartment2: HashSet::from_iter(items[items.len() / 2..].chars()),
        }
    }
}

fn main() {
    let input = include_str!("../input.txt");
    println!("Sum of priorities, part 1: {}", priority_sum(input));
    println!("Part 2: {}", priority_sum_item_types(input));
}

/////////////////////////////////////
// Part 1
/////////////////////////////////////

fn priority_sum(input: &str) -> u32 {
    input
        .lines()
        .filter_map(|s| duplicate_item(s.into()))
        .map(priority)
        .sum()
}

fn duplicate_item(backpack: Backpack) -> Option<char> {
    backpack
        .compartment1
        .intersection(&backpack.compartment2)
        .next()
        .copied()
}

fn priority(c: char) -> u32 {
    let cv = c as u32;
    if cv >= 97 {
        // Lowercase letter
        // Lowercase item types a through z have priorities 1 through 26.
        cv - 96
    } else {
        // Uppercase letter
        // Uppercase item types A through Z have priorities 27 through 52.
        cv - 38
    }
}

/////////////////////////////////////
// Part 2
/////////////////////////////////////

fn priority_sum_item_types(input: &str) -> u32 {
    let mut sum = 0;
    let mut lines = input.lines();
    loop {
        if let (Some(b1), Some(b2), Some(b3)) = (lines.next(), lines.next(), lines.next()) {
            let backpacks: Vec<Backpack> = vec![b1.into(), b2.into(), b3.into()];
            if let Some(item) = shared_item(&backpacks) {
                sum += priority(item);
            }
        } else {
            return sum;
        }
    }
}

fn shared_item(backpacks: &[Backpack]) -> Option<char> {
    fn unique_items(backpack: &Backpack) -> HashSet<char> {
        let mut union = HashSet::new();
        for c in backpack.compartment1.union(&backpack.compartment2) {
            union.insert(*c);
        }
        union
    }

    fn intersect(b1: HashSet<char>, b2: HashSet<char>) -> HashSet<char> {
        let mut intersection = HashSet::new();
        for c in b1.intersection(&b2) {
            intersection.insert(*c);
        }
        intersection
    }

    match backpacks.iter().map(unique_items).reduce(intersect) {
        Some(shared_items) => shared_items.into_iter().next(),
        None => None,
    }
}

#[cfg(test)]
mod test {
    use crate::{
        duplicate_item, priority, priority_sum, priority_sum_item_types, shared_item, Backpack,
    };

    #[test]
    fn item_score() {
        for (c, p) in ('a'..'z').into_iter().zip(1..) {
            assert_eq!(priority(c), p);
        }
        for (c, p) in ('A'..'Z').into_iter().zip(27..) {
            assert_eq!(priority(c), p);
        }
    }

    #[test]
    fn common_item() {
        assert_eq!(duplicate_item("vJrwpWtwJgWrhcsFMMfFFhFp".into()), Some('p'));
    }

    #[test]
    fn common_value_score() {
        let input = include_str!("../sample.txt");
        assert_eq!(priority_sum(input), 157);
    }

    #[test]
    fn shared_item_type() {
        let input1 =
            "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg";
        assert_eq!(
            shared_item(&input1.lines().map(|l| l.into()).collect::<Vec<Backpack>>()),
            Some('r')
        );
    }

    #[test]
    fn sum_priority_item_types() {
        let input = include_str!("../sample.txt");
        assert_eq!(priority_sum_item_types(input), 70);
    }
}
