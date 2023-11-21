use std::collections::{HashSet};

#[derive(Debug, Clone)]
struct Rucksack {
  first_compartment: String,
  second_compartment: String,
}

fn char_to_prio(c: char) -> u64 {
  match c.is_ascii_lowercase() {
    true => (c as u64) - ('a' as u64) + 1,
    false => (c as u64) - ('A' as u64) + 27,
  } 
}

impl Rucksack {
  fn from(s: &str) -> Rucksack {
    assert!(s.len() % 2 == 0, "{} len {} was not divisible by 2", s, s.len());
    let (first, second) = s.split_at(s.len()/2);
    Rucksack { first_compartment: first.chars().collect(), second_compartment: second.chars().collect() }
  }

  fn find_common_item(self) -> Option<char> {
    let s2_charset: HashSet<char> = HashSet::from_iter(self.second_compartment.chars().clone());
    self.first_compartment.chars().find(|c| s2_charset.contains(&c))
  }

  fn combine_compartments(self) -> String {
    self.first_compartment + &self.second_compartment
  }

  fn find_common_item_from_three(self, r2: Rucksack, r3: Rucksack) -> Option<char> {
    let r1_chars: HashSet<char> = HashSet::from_iter(self.combine_compartments().chars());
    let r2_chars: HashSet<char> = HashSet::from_iter(r2.combine_compartments().chars());
    let r3_chars: HashSet<char> = HashSet::from_iter(r3.combine_compartments().chars());
    (&(&r1_chars & &r2_chars) & &r3_chars).iter().next().copied()
  }
}

fn solve1(data: &str) -> u64 {
  let rucksacks = data.lines().map(Rucksack::from);
  let commons = rucksacks.filter_map(|r| r.find_common_item());
  let prios = commons.map(char_to_prio);
  prios.sum()
}

fn solve2(data: &str) -> u64 {
  let lines: Vec<&str> = data.split("\n").collect(); 
  assert!(lines.len() % 3 == 0);
  let groups: Vec<Vec<Rucksack>> = lines
    .chunks(3)
    .map(|c| c.iter().map(|s| Rucksack::from(s)).collect()).collect();
  let badges = groups.iter()
    .filter_map(|g| g[0].clone().find_common_item_from_three(g[1].clone(), g[2].clone()));
  let prios = badges.map(char_to_prio);
  prios.sum()
}

fn main() {
  let data = include_str!("../input.txt");
  println!("Part 1: {}", solve1(data));
  println!("Part 2: {}", solve2(data));
}

#[test]
fn test_part1() {
  let data = include_str!("../example.txt");
  assert_eq!(solve1(data), 157)
}

#[test]
fn test_part2() {
  let data = include_str!("../example.txt");
  assert_eq!(solve2(data), 70);
}
