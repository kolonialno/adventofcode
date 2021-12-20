use itertools::Itertools;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map,
    sequence::{delimited, separated_pair},
    IResult,
};
use std::ops::Add;

#[derive(Clone, Debug)]
enum Number {
    Regular(u64),
    Pair(Box<Number>, Box<Number>),
}

impl Add for Number {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self::Pair(Box::new(self), Box::new(rhs))
    }
}

impl Number {
    fn reduce(self) -> Self {
        let mut number = self;
        loop {
            let (n, exploded) = number.clone().explode(0);
            number = n;
            if exploded.is_some() {
                continue;
            }
            let (n, split) = number.split();
            number = n;
            if !split {
                break;
            }
        }
        number
    }

    // Needed some "inspiration" to get this one right
    fn explode(self, depth: usize) -> (Self, Option<(Option<u64>, Option<u64>)>) {
        match self {
            Self::Regular(_) => (self, None),
            Self::Pair(l, r) => match (*l, *r) {
                (Self::Regular(e_l), Self::Regular(e_r)) if depth >= 4 => {
                    (Self::Regular(0), Some((Some(e_l), Some(e_r))))
                }
                (l, r) => match l.explode(depth + 1) {
                    (l_e, Some((explode_left, explode_right))) => {
                        let r_added = if let Some(explode_right) = explode_right {
                            // take the right side of the left explode and add the value from left.
                            r.add_to_left(explode_right)
                        } else {
                            r
                        };
                        // return Pair(0, new value from right)
                        (
                            Self::Pair(Box::new(l_e), Box::new(r_added)),
                            Some((explode_left, None)),
                        )
                    }
                    (e_l, None) => match r.explode(depth + 1) {
                        (r_e, Some((explode_left, explode_right))) => {
                            let l_added = if let Some(explode_left) = explode_left {
                                // take the left side of the right explode and add value from right
                                e_l.add_to_right(explode_left)
                            } else {
                                e_l
                            };
                            (
                                Self::Pair(Box::new(l_added), Box::new(r_e)),
                                Some((None, explode_right)),
                            )
                        }
                        (r_e, None) => (Self::Pair(Box::new(e_l), Box::new(r_e)), None),
                    },
                },
            },
        }
    }

    fn split(self) -> (Self, bool) {
        match self {
            Number::Regular(n) if n >= 10 => (
                Number::Pair(
                    Box::new(Number::Regular(n / 2)),
                    Box::new(Number::Regular((n + 1) / 2)),
                ),
                true,
            ),
            Number::Regular(_) => (self, false),
            Number::Pair(l, r) => {
                let (l_n, l_split) = l.split();
                if l_split {
                    (Number::Pair(Box::new(l_n), r), true)
                } else {
                    let (r_n, r_split) = r.split();
                    (Number::Pair(Box::new(l_n), Box::new(r_n)), r_split)
                }
            }
        }
    }

    fn add_to_left(self, val: u64) -> Self {
        match self {
            Number::Regular(n) => Number::Regular(n + val),
            Number::Pair(l, r) => Number::Pair(Box::new(l.add_to_left(val)), r),
        }
    }

    fn add_to_right(self, val: u64) -> Self {
        match self {
            Number::Regular(n) => Number::Regular(n + val),
            Number::Pair(l, r) => Number::Pair(l, Box::new(r.add_to_right(val))),
        }
    }

    fn magnitude(&self) -> u64 {
        match self {
            Number::Regular(n) => *n,
            Number::Pair(l, r) => 3 * l.magnitude() + 2 * r.magnitude(),
        }
    }
}

fn parse_number(input: &str) -> IResult<&str, Number> {
    alt((
        map(nom::character::complete::u64, Number::Regular),
        map(
            delimited(
                tag("["),
                separated_pair(parse_number, tag(","), parse_number),
                tag("]"),
            ),
            |(l, r)| Number::Pair(Box::new(l), Box::new(r)),
        ),
    ))(input)
}

fn main() {
    let input = include_str!("../input.txt")
        .lines()
        .map(|line| parse_number(&line).unwrap().1)
        .collect_vec();

    let res1 = input
        .clone()
        .into_iter()
        .reduce(|l, r| (l + r.clone()).reduce())
        .unwrap();
    println!("{}", res1.magnitude());

    let res2 = input
        .clone()
        .into_iter()
        .permutations(2)
        .map(|permutation| {
            permutation
                .into_iter()
                .reduce(|l, r| (l + r.clone()).reduce())
                .unwrap()
                .magnitude()
        });
    println!("{}", res2.max().unwrap());
}
