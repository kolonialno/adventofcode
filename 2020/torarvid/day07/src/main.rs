use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::format;
use std::fs;
use std::str::Lines;

fn main() {
    let filename = "input.txt";
    let contents =
        fs::read_to_string(filename).expect(&format!("Failed to read file {}", filename));
    println!("Part1: {}", solve_part1(contents.lines()));
    println!("Part2: {}", solve_part2(contents.lines()));
}

#[derive(Debug, Clone)]
struct Bag {
    amount: u16,
    material: String,
    color: String,
}

impl Bag {
    fn to_key(&self) -> String {
        format!("{}-{}", self.material, self.color)
    }
}

fn solve_part1(input: Lines) -> usize {
    let mut map = HashMap::new();
    for line in input {
        let mut parts = line.split(" contain ");
        let parent = parse_bag(parts.next().unwrap()).unwrap();
        let children = parts.next().unwrap().split(", ").flat_map(parse_bag);
        children.for_each(|child| {
            let entry = map.entry(child.to_key()).or_insert(Vec::<Bag>::new());
            entry.push(parent.clone());
        });
    }
    let mut set = HashSet::new();

    let mut queue = VecDeque::new();
    queue.push_back(Bag {
        amount: 1,
        material: String::from("shiny"),
        color: String::from("gold"),
    });

    while !queue.is_empty() {
        let parent = queue.pop_front().unwrap();
        map.get(&parent.to_key()).map(|children| {
            for child in children {
                set.insert(child.to_key());
                queue.push_back(child.clone());
            }
        });
    }
    set.len()
}

fn solve_part2(input: Lines) -> u16 {
    let mut map = HashMap::new();
    for line in input {
        let mut parts = line.split(" contain ");
        let parent = parse_bag(parts.next().unwrap()).unwrap();
        let children = parts.next().unwrap().split(", ").flat_map(parse_bag);
        map.insert(parent.to_key(), children.collect::<Vec<Bag>>());
    }
    let my_bag = Bag {
        amount: 1,
        material: String::from("shiny"),
        color: String::from("gold"),
    };
    let bags_including_my_bag = bags_in_map(&map, &my_bag);
    bags_including_my_bag - 1
}

fn bags_in_map(map: &HashMap<String, Vec<Bag>>, bag: &Bag) -> u16 {
    let mut bags = 0;
    if let Some(children) = map.get(&bag.to_key()) {
        children.iter().for_each(|c| bags += bags_in_map(map, c));
    }
    bag.amount + bag.amount * bags
}

fn parse_bag(input: &str) -> Option<Bag> {
    if input == "no other bags." {
        return None;
    }
    let parts = input.split(" ").collect::<Vec<&str>>();
    let amount = if parts.len() == 4 {
        parts[0].parse::<u16>().unwrap()
    } else if parts[0] == "no" {
        0
    } else {
        1
    };
    Some(Bag {
        amount,
        material: String::from(parts[parts.len() - 3]),
        color: String::from(parts[parts.len() - 2]),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let test_data = String::from(
            "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.",
        );
        assert_eq!(solve_part1(test_data.lines()), 4);
        assert_eq!(solve_part2(test_data.lines()), 32);
        let test_data2 = String::from(
            "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.",
        );
        assert_eq!(solve_part2(test_data2.lines()), 126);
    }
}
