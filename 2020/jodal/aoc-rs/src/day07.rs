use daggy::{Dag, EdgeIndex, NodeIndex, Walker};
use itertools::Itertools;
use regex::Regex;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter::Extend;
use std::str::Lines;

pub fn solve_a(bag_rules: &BagRules) -> usize {
    let bag_color = BagColor("shiny gold".to_owned());
    let node_index = bag_rules.nodes.get(&bag_color).expect("Unknown bag color");

    bag_rules
        .walk(*node_index, |n| bag_rules.get_parents(*n))
        .iter()
        .map(|(_, n)| n)
        .unique()
        .count()
}

pub fn solve_b(bag_rules: &BagRules) -> u32 {
    let bag_color = BagColor("shiny gold".to_owned());
    let node_index = bag_rules.nodes.get(&bag_color).expect("Unknown bag color");
    bag_rules.num_within(*node_index)
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct BagColor(String);

#[derive(Debug)]
pub struct BagRules {
    nodes: HashMap<BagColor, NodeIndex>,
    dag: Dag<BagColor, u32, u32>,
}

impl BagRules {
    pub fn from_lines(lines: Lines) -> BagRules {
        let mut bag_rules = BagRules {
            nodes: HashMap::new(),
            dag: Dag::new(),
        };
        let bag_spec_re = Regex::new(r"^(\d+) (\w+ \w+) bags?\.?$").unwrap();

        for line in lines {
            let parts: Vec<&str> = line.split(" bags contain ").collect();

            let parent_bag_color = BagColor(parts.get(0).unwrap().to_string());
            let parent_node_index = bag_rules.get_or_insert_node(&parent_bag_color);

            for child_bag_spec in parts.get(1).unwrap().split(", ") {
                match bag_spec_re.captures(&child_bag_spec) {
                    Some(caps) => {
                        let num_bags = caps[1].parse::<u32>().unwrap();
                        let child_bag_color = BagColor(caps[2].to_owned());
                        let child_node_index = bag_rules.get_or_insert_node(&child_bag_color);
                        bag_rules
                            .dag
                            .add_edge(parent_node_index, child_node_index, num_bags)
                            .unwrap();
                    }
                    None => {} // No other bags within.
                }
            }
        }

        bag_rules
    }

    fn get_or_insert_node(&mut self, bag_color: &BagColor) -> NodeIndex {
        match self.nodes.entry(bag_color.clone()) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let node_index = self.dag.add_node(bag_color.clone());
                entry.insert(node_index);
                node_index
            }
        }
    }

    fn get_parents(&self, node_index: NodeIndex) -> Vec<(EdgeIndex, NodeIndex)> {
        self.dag.parents(node_index).iter(&self.dag).collect()
    }

    fn get_children(&self, node_index: NodeIndex) -> Vec<(EdgeIndex, NodeIndex)> {
        self.dag.children(node_index).iter(&self.dag).collect()
    }

    fn walk<F>(&self, node_index: NodeIndex, walk_fn: F) -> Vec<(EdgeIndex, NodeIndex)>
    where
        F: Fn(&NodeIndex) -> Vec<(EdgeIndex, NodeIndex)>,
    {
        let mut result = Vec::new();
        let mut to_visit: Vec<(EdgeIndex, NodeIndex)> = walk_fn(&node_index);
        while let Some((e, n)) = to_visit.pop() {
            to_visit.extend(walk_fn(&n));
            result.push((e, n));
        }
        result
    }

    fn num_within(&self, node_index: NodeIndex) -> u32 {
        self.get_children(node_index)
            .iter()
            .map(|(e, n)| self.dag.edge_weight(*e).unwrap() * (1 + self.num_within(*n)))
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_a() {
        let input = String::from(
            r"light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.",
        );
        let bag_rules = BagRules::from_lines(input.lines());
        assert_eq!(solve_a(&bag_rules), 4);
    }

    #[test]
    fn example_b() {
        let input = String::from(
            r"shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.",
        );
        let bag_rules = BagRules::from_lines(input.lines());
        assert_eq!(solve_b(&bag_rules), 126);
    }
}
