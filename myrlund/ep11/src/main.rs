use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    str::FromStr,
};

use anyhow::Context;
use lazy_static::lazy_static;
use regex::Regex;

static RELIEF_FACTOR: u128 = 3;

#[derive(Debug, Clone)]
struct Item(u128);

impl Item {
    fn apply_relief(&mut self) {
        self.0 /= RELIEF_FACTOR;
    }
}

#[derive(Debug)]
enum Operation {
    AddConst(u128),
    MulConst(u128),
    AddSelf,
    MulSelf,
}

impl FromStr for Operation {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^new = old (?P<op>[\*\+]) (?P<obj>.+)$").unwrap();
        }

        let cap = RE.captures(s).unwrap();

        let op = cap.name("op").unwrap().as_str();
        let obj = cap.name("obj").unwrap().as_str();

        Ok(match op {
            "+" => match obj {
                "old" => Self::AddSelf,
                s => Self::AddConst(s.parse().context("unexpected added argument")?),
            },
            "*" => match obj {
                "old" => Self::MulSelf,
                s => Self::MulConst(s.parse().context("unexpected multiplied argument")?),
            },
            _ => unreachable!(),
        })
    }
}

impl Operation {
    fn execute(&self, item: &Item) -> u128 {
        match self {
            Operation::AddConst(v) => item.0 + v,
            Operation::AddSelf => item.0 * 2,
            Operation::MulConst(v) => item.0 * v,
            Operation::MulSelf => item.0.pow(2),
        }
    }
}

#[derive(Debug)]
struct TestOperation {
    test_divisible_by: usize,

    /// Monkey ids
    outcome_true: usize,
    outcome_false: usize,
}

impl TestOperation {
    fn decide_next_monkey(&self, item: &Item) -> usize {
        let outcome = item.0 % (self.test_divisible_by as u128) == 0;
        if outcome {
            self.outcome_true
        } else {
            self.outcome_false
        }
    }
}

#[derive(Debug)]
struct Monkey {
    id: usize,
    items: RefCell<VecDeque<Item>>,
    operation: Operation,
    test_operation: TestOperation,

    inspect_count: usize,
}

impl FromStr for Monkey {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref RE: Regex = Regex::new(
                r"^Monkey (?P<monkey_id>\d+):
  Starting items: (?P<items>.*)
  Operation: (?P<operation>.*)
  Test: divisible by (?P<divisible_by>\d+)
    If true: throw to monkey (?P<outcome_true>\d+)
    If false: throw to monkey (?P<outcome_false>\d+)"
            )
            .unwrap();
        }

        let cap = RE.captures(s).context("No Monkey match for given string")?;

        Ok(Monkey {
            id: cap.name("monkey_id").unwrap().as_str().parse().unwrap(),
            items: RefCell::new(
                cap.name("items")
                    .unwrap()
                    .as_str()
                    .split(", ")
                    .map(|n| Item(n.parse().unwrap()))
                    .collect(),
            ),
            operation: cap.name("operation").unwrap().as_str().parse().unwrap(),
            test_operation: TestOperation {
                test_divisible_by: cap.name("divisible_by").unwrap().as_str().parse().unwrap(),
                outcome_true: cap.name("outcome_true").unwrap().as_str().parse().unwrap(),
                outcome_false: cap.name("outcome_false").unwrap().as_str().parse().unwrap(),
            },
            inspect_count: 0,
        })
    }
}

impl Monkey {
    fn inspect_and_toss_item(
        &mut self,
        apply_relief: bool,
        divisor: u128,
    ) -> Option<(Item, usize)> {
        let mut item = self.items.borrow_mut().pop_front()?;

        self.inspect_count += 1;

        item.0 = self.operation.execute(&item) % divisor;
        if apply_relief {
            item.apply_relief();
        }

        let next_monkey_id = self.test_operation.decide_next_monkey(&item);
        Some((item, next_monkey_id))
    }

    fn receive_item(&mut self, item: Item) {
        self.items.borrow_mut().push_back(item);
    }
}

fn simulate_monkey_business(s: &str, rounds: usize, apply_relief: bool) -> i128 {
    // Store monkey data in a HashMap with RefCells to be able to mutate
    // multiple monkeys' item vecs simultaneously.
    let monkeys: HashMap<usize, RefCell<Monkey>> = s
        .split("\n\n")
        .map(|monkey_str| monkey_str.parse::<Monkey>().unwrap())
        .map(|monkey| (monkey.id, RefCell::new(monkey)))
        .collect();
    let monkey_ids = {
        let mut m = monkeys.keys().collect::<Vec<_>>();
        m.sort();
        m
    };

    // Calculate the product of the divisors used to pass the items onwards.
    let multiple: i128 = monkeys
        .values()
        .map(|m| m.borrow().test_operation.test_divisible_by as i128)
        .product();
    let multiple = multiple as u128;

    // Run through the rounds
    for _ in 1..=rounds {
        for monkey_id in monkey_ids.iter() {
            let mut monkey = monkeys.get(monkey_id).unwrap().borrow_mut();

            while let Some((item, next_monkey_id)) =
                monkey.inspect_and_toss_item(apply_relief, multiple)
            {
                let mut next_monkey = monkeys.get(&next_monkey_id).unwrap().borrow_mut();
                next_monkey.receive_item(item);
            }
        }
    }

    // Find the two largest inspect_counts and multiply them
    let mut counts = monkeys
        .values()
        .map(|m| m.borrow().inspect_count)
        .collect::<Vec<_>>();
    counts.sort();

    counts.iter().rev().take(2).map(|&n| n as i128).product()
}

fn run_part_one(s: &str) -> i128 {
    simulate_monkey_business(s, 20, true)
}

fn run_part_two(s: &str) -> i128 {
    simulate_monkey_business(s, 10000, false)
}

fn main() {
    let input = include_str!("../input.txt");

    let part_one = run_part_one(input);
    println!("Part 1: {part_one}");

    let part_two = run_part_two(input);
    println!("Part 2: {part_two}");
}

#[cfg(test)]
mod tests {
    use crate::{run_part_one, run_part_two};

    static SAMPLE: &str = include_str!("../sample.txt");

    #[test]
    fn sample_part_one_is_correct() {
        assert_eq!(run_part_one(SAMPLE), 10605);
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two(SAMPLE), 2713310158);
    }
}
