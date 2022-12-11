use itertools::Itertools;
use std::collections::VecDeque;

fn main() {
    let mut monkeys = real_monkeys();
    // all divisors are prime
    let lcm = monkeys.iter().map(|m| m.test).product::<u128>();
    for _ in 0..10000 {
        for i in 0..monkeys.len() {
            while let Some(item) = monkeys[i].items.pop_front() {
                let targets = monkeys[i].targets;
                let v = match monkeys[i].operation {
                    Ops::Mul(o) => (o * item) % lcm,
                    Ops::Add(o) => (o + item) % lcm,
                    Ops::Square => (item * item) % lcm,
                };
                match v % monkeys[i].test {
                    0 => monkeys[targets.0].items.push_back(v),
                    _ => monkeys[targets.1].items.push_back(v),
                };
                monkeys[i].inspected += 1
            }
        }
    }
    let r = monkeys
        .into_iter()
        .map(|m| m.inspected)
        .sorted()
        .rev()
        .take(2)
        .product::<u128>();
    println!("part 1: {:?}", r)
}

#[derive(Debug)]
enum Ops {
    Add(u128),
    Mul(u128),
    Square,
}

#[derive(Debug)]
struct Monkey {
    items: VecDeque<u128>,
    operation: Ops,
    test: u128,
    targets: (usize, usize),
    inspected: u128,
}

fn real_monkeys() -> Vec<Monkey> {
    vec![
        Monkey {
            items: VecDeque::from([83, 62, 93]),
            operation: Ops::Mul(17),
            test: 2,
            targets: (1, 6),
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([90, 55]),
            operation: Ops::Add(1),
            test: 17,
            targets: (6, 3),
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([91, 78, 80, 97, 79, 88]),
            operation: Ops::Add(3),
            test: 19,
            targets: (7, 5),
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([64, 80, 83, 89, 59]),
            operation: Ops::Add(5),
            test: 3,
            targets: (7, 2),
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([98, 92, 99, 51]),
            operation: Ops::Square,
            test: 5,
            targets: (0, 1),
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([68, 57, 95, 85, 98, 75, 98, 75]),
            operation: Ops::Add(2),
            test: 13,
            targets: (4, 0),
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([74]),
            operation: Ops::Add(4),
            test: 7,
            targets: (3, 2),
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([68, 64, 60, 68, 87, 80, 82]),
            operation: Ops::Mul(19),
            test: 11,
            targets: (4, 5),
            inspected: 0,
        },
    ]
}

fn test_monkeys() -> Vec<Monkey> {
    vec![
        Monkey {
            items: VecDeque::from([79, 98]),
            operation: Ops::Mul(19),
            test: 23,
            targets: (2, 3),
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([54, 65, 75, 74]),
            operation: Ops::Add(6),
            test: 19,
            targets: (2, 0),
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([79, 60, 97]),
            operation: Ops::Square,
            test: 13,
            targets: (1, 3),
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([74]),
            operation: Ops::Add(3),
            test: 17,
            targets: (0, 1),
            inspected: 0,
        },
    ]
}
