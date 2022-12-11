use std::collections::VecDeque;

use regex::Regex;

struct Monkey {
    items: VecDeque<u64>,
    op: Box<dyn Fn(u64) -> u64>,
    test: Box<dyn Fn(u64) -> usize>,
    inspected: usize,
}

fn main() {
    let input = include_str!("input.txt");
    let items_re = Regex::new(r"Starting items: (((\d*)(, )?)*)").unwrap();
    let op_re = Regex::new(r"Operation: new = old (.*)\n").unwrap();
    let test_re = Regex::new(r"divisible by (\d*)[^\d]*(\d*)[^\d]*(\d*)").unwrap();

    let mut shared_factor = 1;
    let mut monkeys: Vec<Monkey> = input
        .split("\n\n")
        .map(|monkey| {
            let item_cap = items_re.captures(monkey).unwrap();
            let op_cap = op_re.captures(monkey).unwrap();
            let op = op_cap[1].split_once(" ").unwrap();
            let test_cap = test_re.captures(monkey).unwrap();
            let test: Vec<usize> = (1..=3)
                .map(|i| usize::from_str_radix(&test_cap[i], 10).unwrap())
                .collect();
            shared_factor *= test[0] as u64;
            Monkey {
                items: VecDeque::from(
                    item_cap[1]
                        .split(", ")
                        .map(|e| u64::from_str_radix(e, 10).unwrap())
                        .collect::<Vec<u64>>(),
                ),
                op: {
                    match op {
                        ("+", "old") => Box::new(|o| o + o),
                        ("*", "old") => Box::new(|o| o * o),
                        ("+", d) => {
                            let num = u64::from_str_radix(d, 10).unwrap();
                            Box::new(move |o| o + num)
                        }
                        ("*", d) => {
                            let num = u64::from_str_radix(d, 10).unwrap();
                            Box::new(move |o| o * num)
                        }
                        _ => panic!("This shouldnt happen"),
                    }
                },
                test: Box::new(move |a| test[if a % (test[0] as u64) == 0 { 1 } else { 2 }]),
                inspected: 0,
            }
        })
        .collect();

    for _round in 0..10000 {
        for i in 0..monkeys.len() {
            monkeys[i].inspected += monkeys[i].items.len();
            while monkeys[i].items.len() != 0 {
                let item = monkeys[i].items.pop_front().unwrap();
                let new_item = (&monkeys[i].op)(item) % shared_factor;
                let target_monkey = (&monkeys[i].test)(new_item);
                monkeys[target_monkey].items.push_back(new_item);
            }
        }
    }

    let mut inspections: Vec<usize> = monkeys.iter().map(|m| m.inspected).collect();
    inspections.sort();
    dbg!(inspections.iter().rev().take(2).fold(1, |a, b| a * b));
}
