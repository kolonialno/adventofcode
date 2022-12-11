use std::{cmp::Reverse, fmt::Display, fs};

use helpers::get_input_file;

type Item = usize;

#[derive(Clone, Copy)]
enum Operand {
    Var,
    Constant(Item),
}

#[derive(Clone, Copy)]
enum Op {
    Add,
    Multiply,
}

#[derive(Clone, Copy)]
struct Operation {
    lhs: Operand,
    rhs: Operand,
    op: Op,
}

impl Operation {
    fn parse(input: &str) -> Self {
        let parts = input
            .split_whitespace()
            .map(|x| x.into())
            .collect::<Vec<String>>();
        let lhs: Operand = if parts[0] == "old" {
            Operand::Var {}
        } else {
            Operand::Constant(parts[0].parse::<Item>().unwrap())
        };

        let rhs = if parts[2] == "old" {
            Operand::Var {}
        } else {
            Operand::Constant(parts[2].parse::<Item>().unwrap())
        };

        let op = if parts[1] == "+" {
            Op::Add
        } else {
            Op::Multiply
        };

        Self { lhs, rhs, op }
    }

    fn execute(&self, item: Item) -> Item {
        let lhs = match self.lhs {
            Operand::Var => item,
            Operand::Constant(c) => c,
        };
        let rhs = match self.rhs {
            Operand::Var => item,
            Operand::Constant(c) => c,
        };

        match self.op {
            Op::Add => lhs + rhs,
            Op::Multiply => lhs * rhs,
        }
    }
}

#[derive(Clone, Copy)]
struct Test {
    divisor: usize,
    monkey_true: usize,
    monkey_false: usize,
}

impl Test {
    fn parse(lines: &[String]) -> Self {
        Self {
            divisor: lines[0]
                .split_whitespace()
                .rev()
                .take(1)
                .collect::<String>()
                .parse::<Item>()
                .unwrap(),
            monkey_true: lines[1]
                .chars()
                .rev()
                .take(1)
                .collect::<String>()
                .parse::<usize>()
                .unwrap(),
            monkey_false: lines[2]
                .chars()
                .rev()
                .take(1)
                .collect::<String>()
                .parse::<usize>()
                .unwrap(),
        }
    }

    fn target(&self, item: Item) -> usize {
        if item % self.divisor == 0 {
            self.monkey_true
        } else {
            self.monkey_false
        }
    }
}

#[derive(Clone)]
struct Monkey {
    nr: usize,
    items: Vec<Item>,
    operation: Operation,
    test: Test,
    inspected_items: usize,
}

impl Monkey {
    fn parse(input: &str) -> Self {
        let lines: Vec<String> = input
            .lines()
            .map(|x| x.split_once(':').unwrap().1.into())
            .collect::<Vec<String>>();

        Self {
            nr: input
                .chars()
                .skip(7)
                .take(1)
                .collect::<String>()
                .parse::<usize>()
                .unwrap(),
            items: lines[1]
                .split(',')
                .into_iter()
                .map(|x| x.trim().parse::<Item>().unwrap())
                .collect(),
            operation: Operation::parse(lines[2].split_once('=').unwrap().1),
            test: Test::parse(&lines[3..]),
            inspected_items: 0,
        }
    }

    fn inspect_items_1(&mut self) -> Vec<(usize, Item)> {
        let mut targets = Vec::default();
        for &item in self.items.iter() {
            let new_item = self.operation.execute(item) / 3;
            targets.push((self.test.target(new_item), new_item));
            self.inspected_items += 1;
        }
        self.items.clear();
        targets
    }

    fn inspect_items_2(&mut self, divisor: usize) -> Vec<(usize, Item)> {
        let mut targets = Vec::default();
        for &item in self.items.iter() {
            let new_item = self.operation.execute(item) % divisor;
            targets.push((self.test.target(new_item), new_item));
            self.inspected_items += 1;
        }
        self.items.clear();
        targets
    }
}

impl Display for Monkey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Monkey {} inspected {} items.",
            self.nr, self.inspected_items,
        )
    }
}

fn main() {
    let input = fs::read_to_string(get_input_file()).unwrap();
    // let input = fs::read_to_string("11/test.txt").unwrap();
    let monkeys = input
        .split("\n\n")
        .map(Monkey::parse)
        .collect::<Vec<Monkey>>();

    let mut monkeys_1 = monkeys.clone();
    for _ in 0..20 {
        for index in 0..monkeys_1.len() {
            let targets = monkeys_1[index].inspect_items_1();
            for target in targets {
                monkeys_1[target.0].items.push(target.1);
            }
        }
    }
    monkeys_1.sort_by_key(|x| Reverse(x.inspected_items));
    println!(
        "Monkey business 1: {}",
        monkeys_1
            .iter()
            .take(2)
            .map(|x| x.inspected_items)
            .reduce(|accum, item| accum * item)
            .unwrap()
    );

    let mut monkeys_2 = monkeys;
    let divisor = monkeys_2
        .iter()
        .map(|x| x.test.divisor)
        .reduce(|acc, e| acc * e)
        .unwrap();
    for i in 0..10000 {
        for index in 0..monkeys_2.len() {
            let targets = monkeys_2[index].inspect_items_2(divisor);
            for target in targets {
                monkeys_2[target.0].items.push(target.1);
            }
        }
        if i == 0 || i == 19 || i == 999 || i == 9999 {
            println!("= After round {} ==", i + 1);
            for monkey in monkeys_2.iter() {
                println!("{}", monkey);
            }
            println!();
        }
    }
    monkeys_2.sort_by_key(|x| Reverse(x.inspected_items));
    println!(
        "Monkey business 2: {}",
        monkeys_2
            .iter()
            .take(2)
            .map(|x| x.inspected_items as u128)
            .reduce(|accum, item| accum * item)
            .unwrap()
    );
}
