use std::collections::HashSet;

// Couldn't figure out how to initizlie this from a range 😥
// const ITEMS: Vec<char> = ['a'..='z', 'A'..='Z'].concat();
const ITEM_ORDER: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

fn item_value(item: char) -> usize {
    ITEM_ORDER.find(item).expect("Invalid item") + 1
}

/// Prase a line of rugsack items, finding the value of the common items
fn find_common_item_in_rugsack(rugsack: &str) -> Option<usize> {
    let items: Vec<_> = rugsack.chars().collect();
    // Rugsack always has an equal number of items in its compartments,
    // so the number of items must be an even number.
    assert_eq!(items.len() % 2, 0);
    let (first, second) = items.split_at(items.len() / 2);
    for item in first {
        if second.contains(item) {
            return Some(item_value(item.clone()));
        }
    }
    None
    // Alternative approach:
    // let first_set: HashSet<char> = first.iter().copied().collect();
    // let second_set: HashSet<char> = second.iter().copied().collect();
    // let common_item = first_set.intersection(&second_set).next().copied().expect("No common items");
    // item_value(common_item)
}

fn solve_part1(input: &str) -> usize {
    input
        .lines()
        .map(|line| find_common_item_in_rugsack(line).expect("No common items in rugsacks"))
        .reduce(|sum, item| sum + item)
        .expect("No items?")
}

fn find_common_item_in_group(first: &str, second: &str, third: &str) -> usize {
    let first: HashSet<char> = first.chars().collect();
    let second: HashSet<char> = second.chars().collect();
    let third: HashSet<char> = third.chars().collect();

    let common_items: HashSet<_> = first.intersection(&second).copied().collect();
    let common_item = common_items
        .intersection(&third)
        .next()
        .copied()
        .expect("No common items");

    item_value(common_item)
}

fn find_common_item_in_group_v2(items: Vec<&str>) -> usize {
    let common_items = items
        .iter()
        .map(|rugsack| rugsack.chars().collect::<HashSet<char>>())
        .reduce(|acc, this| acc.intersection(&this).copied().collect())
        .expect("No rugsacks");
    let common_item = common_items
        .iter()
        .next()
        .copied()
        .expect("No common items");
    item_value(common_item)
}

fn solve_part2(input: &str) -> usize {
    let lines: Vec<_> = input.lines().collect();
    lines
        .chunks(3)
        .map(|items| find_common_item_in_group_v2(items.into()))
        .reduce(|sum, cur| sum + cur)
        .expect("No rugsacks?")
}

fn main() {
    let input = include_str!("./input.txt");
    let sum = solve_part1(input);
    println!("Part 1: {sum}");
    let sum = solve_part2(input);
    println!("Part 2: {sum}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part1(input), 157);
    }

    #[test]
    fn test_part2() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part2(input), 70);
    }
}
