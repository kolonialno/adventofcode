type Result<T> = std::result::Result<T, anyhow::Error>;

use anyhow::bail;
use regex::Regex;
use std::collections::VecDeque;
use std::io::Read;

#[derive(Debug)]
enum Atom {
    OldValue,
    Literal(i64),
}

#[derive(Debug)]
enum Expr {
    Add(Atom, Atom),
    Mul(Atom, Atom),
}

impl Atom {
    fn eval(&self, old_value: i64) -> i64 {
        match *self {
            Atom::OldValue => old_value,
            Atom::Literal(x) => x,
        }
    }
}

impl Expr {
    fn eval(&self, old_value: i64) -> i64 {
        match self {
            Expr::Add(a, b) => a.eval(old_value) + b.eval(old_value),
            Expr::Mul(a, b) => a.eval(old_value) * b.eval(old_value),
        }
    }
}
fn parse_atom(s: &str) -> Result<Atom> {
    match s {
        "old" => Ok(Atom::OldValue),
        _ => Ok(Atom::Literal(s.parse()?)),
    }
}

fn parse_expr(s: &str) -> Result<Expr> {
    let tokens: Vec<&str> = s.split_whitespace().collect();

    assert!(tokens.len() == 3);

    let left = parse_atom(tokens[0])?;
    let right = parse_atom(tokens[2])?;

    match tokens[1] {
        "+" => Ok(Expr::Add(left, right)),
        "*" => Ok(Expr::Mul(left, right)),
        _ => bail!("bad operator: {}", tokens[1]),
    }
}

#[derive(Debug)]
struct Monkey {
    index: usize,
    inspect_count: i64,
    expr: Expr,
    items: VecDeque<i64>,
    divisor: i64,
    target_if_divisible: usize,
    target_if_not_divisible: usize,
}

fn run_round(monkeys: &mut [Monkey], divide_worry: bool) -> Result<()> {
    let product_of_divisors: i64 = monkeys.iter().map(|x| x.divisor).product();

    let mut item_queue: Vec<VecDeque<i64>> = monkeys.iter().map(|_| VecDeque::new()).collect();

    for monkey in monkeys.iter_mut() {
        monkey.items.append(&mut item_queue[monkey.index]);
        item_queue[monkey.index] = VecDeque::new();

        while !monkey.items.is_empty() {
            monkey.inspect_count += 1;

            let worry = monkey.items.pop_front().unwrap();
            let worry = monkey.expr.eval(worry);
            let worry = if divide_worry {
                worry / 3
            } else {
                worry % product_of_divisors
            };
            let divisible = worry % monkey.divisor == 0;

            let target_monkey = if divisible {
                monkey.target_if_divisible
            } else {
                monkey.target_if_not_divisible
            };

            item_queue[target_monkey].push_back(worry);
        }
    }

    for monkey in monkeys.iter_mut() {
        monkey.items.append(&mut item_queue[monkey.index]);
    }

    Ok(())
}

fn run_scenario(spec: &str, rounds: usize, divide: bool) -> Result<i64> {
    let re = Regex::new(r"Monkey (?P<index>[0-9]+):.*\n.*Starting items: (?P<items>.*)\n.*Operation: new = (?P<expr>.*)\n.*Test: divisible by (?P<divisor>[0-9]+)\n.*If true: throw to monkey (?P<yes>[0-9]+).*\n.*If false: throw to monkey (?P<no>[0-9]+)").unwrap();

    let mut monkeys: Vec<Monkey> = spec
        .split("\n\n")
        .map(|text| {
            let captures = re.captures(text.trim()).unwrap();

            Monkey {
                index: captures["index"].parse().unwrap(),
                inspect_count: 0,
                expr: parse_expr(&captures["expr"]).unwrap(),
                items: VecDeque::from_iter(
                    captures["items"]
                        .split(',')
                        .map(|x| x.trim().parse::<i64>().unwrap()),
                ),
                divisor: captures["divisor"].parse().unwrap(),
                target_if_divisible: captures["yes"].parse().unwrap(),
                target_if_not_divisible: captures["no"].parse().unwrap(),
            }
        })
        .collect();

    for _ in 0..rounds {
        run_round(&mut monkeys, divide)?;
    }

    let mut counts: Vec<i64> = monkeys.iter().map(|x| x.inspect_count).collect();
    counts.sort();

    let most_active_counts: Vec<i64> = counts.iter().rev().take(2).copied().collect();

    Ok(most_active_counts.iter().product::<i64>())
}

fn main() -> Result<()> {
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer)?;

    println!("Part A: {}", run_scenario(&buffer, 20, true)?);
    println!("Part A: {}", run_scenario(&buffer, 10_000, false)?);

    Ok(())
}
