#[derive(Debug)]
enum Operation {
    Exponential,
    Multipy(usize),
    Add(usize),
}

impl Operation {
    fn calculate(&self, item: usize) -> usize {
        match self {
            Operation::Exponential => item * item,
            Operation::Multipy(value) => item * value,
            Operation::Add(value) => item + value,
        }
    }
}

struct Monkey {
    items: Vec<usize>,
    // Operation performed on current item.
    // Takes in current worry level and returns a new worry level
    operation: Operation,
    // Test to decide which monkey to throw the item to
    // Takes in worry level and returns monkey id
    test: (usize, usize, usize),
    items_inspected: usize,
}

impl Monkey {
    fn new(items: Vec<usize>, operation: Operation, test: (usize, usize, usize)) -> Self {
        Monkey {
            items,
            operation,
            test,
            items_inspected: 0,
        }
    }

    fn inspect_next(&mut self, modulus: usize) -> Option<(usize, usize)> {
        if self.items.is_empty() {
            return None;
        }
        self.items_inspected += 1;
        let item = self.items.remove(0);
        let item = self.operation.calculate(item);
        let item = item % modulus;
        let remainder = item % self.test.0;
        if remainder == 0 {
            Some((self.test.1, item))
        } else {
            Some((self.test.2, item))
        }
    }
}

fn run_round(monkeys: &mut Vec<Monkey>) {
    let modulus = monkeys.iter().map(|monkey| monkey.test.0).product();
    for i in 0..monkeys.len() {
        while let Some((to_monkey, item)) = monkeys[i].inspect_next(modulus) {
            monkeys[to_monkey].items.push(item);
        }
    }
}

fn solve_part1(mut monkeys: Vec<Monkey>) -> usize {
    for i in 0..10000 {
        run_round(&mut monkeys);
        if i == 0 || i == 19 || (i + 1) % 1000 == 0 {
            println!("Round {i}");
            for (i, monkey) in monkeys.iter().enumerate() {
                println!(
                    "    Monkey {i} ({}) has: {:?}",
                    monkey.items_inspected, monkey.items
                );
            }
        }
    }

    let mut highest = 0;
    let mut second = 0;

    for monkey in monkeys {
        if monkey.items_inspected > highest {
            second = highest;
            highest = monkey.items_inspected;
        } else if monkey.items_inspected > second {
            second = monkey.items_inspected;
        }
    }

    highest * second
}

fn get_monkeys() -> Vec<Monkey> {
    vec![
        Monkey::new(
            vec![76, 88, 96, 97, 58, 61, 67],
            Operation::Multipy(19),
            (3, 2, 3),
        ),
        Monkey::new(
            vec![93, 71, 79, 83, 69, 70, 94, 98],
            Operation::Add(8),
            (11, 5, 6),
        ),
        Monkey::new(
            vec![50, 74, 67, 92, 61, 76],
            Operation::Multipy(13),
            (19, 3, 1),
        ),
        Monkey::new(vec![76, 92], Operation::Add(6), (5, 1, 6)),
        Monkey::new(vec![74, 94, 55, 87, 62], Operation::Add(5), (2, 2, 0)),
        Monkey::new(vec![59, 62, 53, 62], Operation::Exponential, (7, 4, 7)),
        Monkey::new(vec![62], Operation::Add(2), (17, 5, 7)),
        Monkey::new(vec![85, 54, 53], Operation::Add(3), (13, 4, 0)),
    ]
}

fn main() {
    let sum = solve_part1(get_monkeys());
    println!("Part 1: {sum}");
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_monkeys() -> Vec<Monkey> {
        vec![
            Monkey::new(vec![79, 98], Operation::Multipy(19), (23, 2, 3)),
            Monkey::new(vec![54, 65, 75, 74], Operation::Add(6), (19, 2, 0)),
            Monkey::new(vec![79, 60, 97], Operation::Exponential, (13, 1, 3)),
            Monkey::new(vec![74], Operation::Add(3), (17, 0, 1)),
        ]
    }

    #[test]
    fn test_part1() {
        assert_eq!(solve_part1(test_monkeys()), 2713310158);
    }
}
