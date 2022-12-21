use std::collections::HashMap;
type Result<T> = std::result::Result<T, anyhow::Error>;

enum BinOp {
    Plus,
    Minus,
    Times,
    Divide,
}

enum Expr {
    Value(i64),
    Calculation(usize, BinOp, usize),
    Unknown,
}

// Due to the nature of the input there is no need to memoize.

fn calculate(exprs: &[Expr], expr: &Expr) -> Option<i64> {
    match expr {
        Expr::Value(x) => Some(*x),
        Expr::Unknown => None,
        Expr::Calculation(left, op, right) => {
            let left = &exprs[*left];
            let right = &exprs[*right];
            let left_value = calculate(exprs, left)?;
            let right_value = calculate(exprs, right)?;
            Some(match op {
                BinOp::Plus => left_value + right_value,
                BinOp::Minus => left_value - right_value,
                BinOp::Times => left_value * right_value,
                BinOp::Divide => {
                    assert!((left_value % right_value) == 0);
                    left_value / right_value
                }
            })
        }
    }
}

fn find_unknown(exprs: &[Expr], expr: &Expr, target: i64) -> Option<i64> {
    match expr {
        Expr::Unknown => Some(target),
        Expr::Value(_) => {
            panic!("find_unknown() called on literal value");
        }
        Expr::Calculation(left, op, right) => {
            let left = &exprs[*left];
            let right = &exprs[*right];
            let left_value = calculate(exprs, left);
            let right_value = calculate(exprs, right);
            let (expr, target) = match (left_value, op, right_value) {
                (None, _, None) => {
                    // This does not occur in the test input, which makes things easier.
                    panic!("both sides are unknown");
                }
                (Some(_), _, Some(_)) => {
                    // This does not occur in the test input, which makes things easier.
                    panic!("both sides are known");
                }
                (Some(left_value), op, None) => (
                    right,
                    match op {
                        BinOp::Plus => target - left_value,
                        BinOp::Minus => left_value - target,
                        BinOp::Times => {
                            assert!(target % left_value == 0);
                            target / left_value
                        }
                        BinOp::Divide => {
                            assert!(left_value % target == 0);
                            left_value / target
                        }
                    },
                ),
                (None, op, Some(right_value)) => (
                    left,
                    match op {
                        BinOp::Plus => target - right_value,
                        BinOp::Minus => target + right_value,
                        BinOp::Times => {
                            assert!(target % right_value == 0);
                            target / right_value
                        }
                        BinOp::Divide => target * right_value,
                    },
                ),
            };
            find_unknown(exprs, expr, target)
        }
    }
}

fn read_scenario() -> Result<(Vec<Expr>, HashMap<String, usize>)> {
    let mut exprs: Vec<Expr> = Vec::new();
    let mut names: HashMap<String, usize> = HashMap::new();

    let lines = std::io::stdin()
        .lines()
        .collect::<std::result::Result<Vec<String>, _>>()?;

    for line in &lines {
        let name: &str = line.trim().split(':').next().unwrap();
        names.insert(name.to_string(), names.len());
    }

    for line in &lines {
        let tokens: Vec<&str> = line.trim().split(' ').collect();
        exprs.push(match &tokens[1..] {
            [value] => Expr::Value(value.parse()?),
            [left, op, right] => {
                let left = names.get(&left.to_string()).unwrap();
                let right = names.get(&right.to_string()).unwrap();
                let op = match *op {
                    "+" => BinOp::Plus,
                    "-" => BinOp::Minus,
                    "*" => BinOp::Times,
                    "/" => BinOp::Divide,
                    _ => anyhow::bail!("bad operator: {}", op),
                };
                Expr::Calculation(*left, op, *right)
            }
            _ => anyhow::bail!("unable to parse expression: {}", line),
        })
    }

    Ok((exprs, names))
}

fn main() -> Result<()> {
    let (mut exprs, names) = read_scenario()?;

    let root = names["root"];

    println!(
        "Answer to part A: {}",
        calculate(&exprs, &exprs[root]).unwrap()
    );

    exprs[names["humn"]] = Expr::Unknown;

    if let Expr::Calculation(left, _, right) = &exprs[root] {
        let expr = Expr::Calculation(*left, BinOp::Minus, *right);
        println!(
            "Answer to part B: {}",
            find_unknown(&exprs, &expr, 0).unwrap()
        );
    }

    Ok(())
}
