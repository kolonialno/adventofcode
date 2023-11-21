use core::fmt;
use std::cmp::Ordering;

use itertools::{EitherOrBoth::*, Itertools};
use nom::{
    branch::alt, bytes::complete::tag, character::complete::digit1, combinator::map,
    multi::separated_list0, sequence::delimited, IResult,
};

#[derive(Clone, PartialEq)]
enum Item {
    L(Vec<Item>),
    V(i32),
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::L(l) => {
                let mid: Vec<String> = l.iter().map(|e| format!("{:?}", e)).collect();
                write!(f, "[{}]", mid.join(","))
            }
            Item::V(v) => write!(f, "{}", v),
        }
    }
}

fn item(input: &str) -> IResult<&str, Item> {
    let list_maker = map(separated_list0(tag(","), item), |i| Item::L(i));
    let value_maker = map(digit1, |e: &str| Item::V(e.parse().unwrap()));
    alt((delimited(tag("["), list_maker, tag("]")), value_maker))(input)
}

fn check_pair(left: &Item, right: &Item) -> Ordering {
    match (left, right) {
        (Item::V(l), Item::V(r)) => {
            if l < r {
                Ordering::Less
            } else if l > r {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        }
        (Item::L(l), Item::L(r)) => {
            l.into_iter()
                .zip_longest(r)
                .fold(Ordering::Equal, |res, pair| match res {
                    Ordering::Equal => match pair {
                        Both(l, r) => check_pair(l, r),
                        Left(_) => Ordering::Greater,
                        Right(_) => Ordering::Less,
                    },
                    r => r,
                })
        }
        (l, Item::V(r)) => check_pair(l, &Item::L(vec![Item::V(*r)])),
        (Item::V(l), r) => check_pair(&Item::L(vec![Item::V(*l)]), r),
    }
}

fn main() {
    let input = include_str!("input.txt");
    let parsed: Vec<[Item; 2]> = input
        .split("\n\n")
        .map(|pair| pair.split_once("\n").unwrap())
        .map(|(l, r)| [item(l).unwrap().1, item(r).unwrap().1])
        .collect();
    dbg!(parsed
        .iter()
        .enumerate()
        .map(|(i, pair)| match check_pair(&pair[0], &pair[1]) {
            Ordering::Less => i + 1,
            _ => 0,
        })
        .sum::<usize>());

    let mut parsed: Vec<Item> = parsed.into_iter().flatten().collect();
    let d1 = Item::L(vec![Item::L(vec![Item::V(2)])]);
    let d2 = Item::L(vec![Item::L(vec![Item::V(6)])]);
    parsed.push(d1.clone());
    parsed.push(d2.clone());
    parsed.sort_by(|a, b| check_pair(a, b));
    dbg!(
        (parsed.iter().position(|e| e == &d1).unwrap() + 1)
            * (parsed.iter().position(|e| e == &d2).unwrap() + 1)
    );
}
