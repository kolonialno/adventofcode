use anyhow::bail;
use itertools::Itertools;
use std::cmp::Ordering;
type Result<T> = std::result::Result<T, anyhow::Error>;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Value {
    Integer(i32),
    List(Vec<Value>),
}

fn parse_value_from(s: &[char]) -> Result<(Value, usize)> {
    match s[0] {
        '[' => {
            let mut consumed: usize = 1;
            let mut list = Vec::new();

            while consumed < s.len() {
                if s[consumed] == ']' {
                    consumed += 1;
                    break;
                }

                let (value, consumed_by_value) = parse_value_from(&s[consumed..])?;
                consumed += consumed_by_value;
                list.push(value);

                match s[consumed] {
                    ']' => (),
                    ',' => {
                        consumed += 1;
                    }
                    ch => bail!("expected comma or end-of-list, got: {}", ch),
                }
            }

            Ok((Value::List(list), consumed))
        }
        n if n.is_ascii_digit() => {
            let number_string = s
                .iter()
                .take_while(|ch| char::is_ascii_digit(*ch))
                .collect::<String>();

            let n: i32 = number_string.parse()?;
            Ok((Value::Integer(n), number_string.len()))
        }
        token => bail!("unexpected token: {}", token),
    }
}

fn parse_value(s: &str) -> Result<Value> {
    let v: Vec<char> = s.chars().collect();
    let (rv, parsed) = parse_value_from(&v)?;
    if s.len() != parsed {
        bail!("parsed only {} chars out of {}", parsed, s.len());
    }
    Ok(rv)
}

fn compare_values(a: &Value, b: &Value) -> Ordering {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a.cmp(b),
        (Value::List(a), Value::List(b)) => {
            let n = a.len().max(b.len());
            for i in 0..n {
                if i >= a.len() {
                    return Ordering::Less;
                }
                if i >= b.len() {
                    return Ordering::Greater;
                }
                let r = compare_values(&a[i], &b[i]);
                if r != Ordering::Equal {
                    return r;
                }
            }
            Ordering::Equal
        }
        (_, Value::Integer(b)) => {
            let b = Value::List(vec![Value::Integer(*b)]);
            compare_values(a, &b)
        }
        (Value::Integer(a), _) => {
            let a = Value::List(vec![Value::Integer(*a)]);
            compare_values(&a, b)
        }
    }
}

fn main() -> Result<()> {
    let marker_one = parse_value("[[2]]")?;
    let marker_two = parse_value("[[6]]")?;

    let mut index_total: usize = 0;
    let mut values: Vec<Value> = Vec::new();

    for (index, mut linechunk) in std::io::stdin().lines().chunks(3).into_iter().enumerate() {
        let left = parse_value(&linechunk.next().unwrap()?)?;
        let right = parse_value(&linechunk.next().unwrap()?)?;
        let result = compare_values(&left, &right);

        if let Some(blank_line) = linechunk.next() {
            assert!(blank_line.unwrap().trim().is_empty());
        }
        assert!(linechunk.next().is_none());

        if result != Ordering::Greater {
            index_total += index + 1;
        }

        values.push(left);
        values.push(right);
    }

    values.push(marker_one.clone());
    values.push(marker_two.clone());

    values.sort_by(compare_values);
    let (marker_one_index, _) = values
        .iter()
        .enumerate()
        .find(|(_, x)| **x == marker_one)
        .unwrap();
    let (marker_two_index, _) = values
        .iter()
        .enumerate()
        .find(|(_, x)| **x == marker_two)
        .unwrap();

    println!("1-based index of marker one: {}", marker_one_index + 1);
    println!("1-based index of marker two: {}", marker_two_index + 1);

    println!();
    println!("Sum of 1-based indices: {}", index_total);

    let answer = (marker_one_index + 1) * (marker_two_index + 1);
    println!("Product of 1-based indices of markers: {}", answer);

    Ok(())
}
