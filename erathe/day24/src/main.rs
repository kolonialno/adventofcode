use itertools::Itertools;
use std::{
    collections::{HashMap, VecDeque},
    str::FromStr,
};

fn get_rhs(a: Arg, y: &HashMap<Mem, i64>) -> i64 {
    match a {
        Arg::Num(n) => n,
        Arg::Mem(c) => *y.get(&Mem(c)).unwrap(),
    }
}

fn main() {
    let mut young_al = include_str!("../input.txt").parse::<Alu>().unwrap();

    for a in (11111111111111i64..99999999999999i64).rev() {
        if a % 10000000 == 0 {
            println!("{}", a);
        }
        let instructions = young_al.instructions.clone();
        let t = a
            .to_string()
            .chars()
            .map(|c| c.to_digit(10).unwrap() as i64)
            .collect_vec();
        if t.contains(&0) {
            continue;
        }

        let mut n = VecDeque::<i64>::from(t.clone());

        for i in instructions {
            // println!("{:?}", i);
            match i.operation {
                Op::Inp => match n.pop_front() {
                    Some(v) => {
                        // println!("---------------");
                        young_al.memory.insert(i.l, v);
                    }
                    None => panic!("Number queue emptied before plan"),
                },
                Op::Add => {
                    let rhs = get_rhs(i.r, &young_al.memory);
                    *young_al.get_mut(i.l) = young_al.get(i.l) + rhs;
                }
                Op::Mul => {
                    let rhs = get_rhs(i.r, &young_al.memory);
                    *young_al.get_mut(i.l) = young_al.get(i.l) * rhs;
                }
                Op::Div => {
                    let rhs = get_rhs(i.r, &young_al.memory);
                    if rhs == 0 {
                        println!("broke");
                        break;
                    }
                    *young_al.get_mut(i.l) = young_al.get(i.l) / rhs;
                }
                Op::Mod => {
                    let rhs = get_rhs(i.r, &young_al.memory);
                    if rhs == 0 {
                        println!("broke");
                        break;
                    }
                    *young_al.get_mut(i.l) = young_al.get(i.l) % rhs;
                }
                Op::Eql => {
                    let rhs = get_rhs(i.r, &young_al.memory);
                    *young_al.get_mut(i.l) = if young_al.get(i.l) == rhs { 1 } else { 0 };
                }
            };
            // println!("{:?}", young_al.memory);
        }

        if *young_al.memory.get(&Mem('z')).unwrap() == 0 {
            println!("{}", a);
            break;
        }
        young_al.reset();
    }
}

#[derive(Clone, Debug)]
enum Op {
    Inp,
    Add,
    Mul,
    Div,
    Mod,
    Eql,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Mem(char);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Arg {
    Num(i64),
    Mem(char),
}

#[derive(Clone, Debug)]
struct Inst {
    operation: Op,
    l: Mem,
    r: Arg,
}

impl Inst {
    fn new(operation: Op, l: Mem, r: Option<Arg>) -> Self {
        let r = match r {
            Some(a) => a,
            None => Arg::Num(0),
        };
        Self { operation, l, r }
    }
}

#[derive(Debug)]
struct Alu {
    instructions: Vec<Inst>,
    memory: HashMap<Mem, i64>,
}

impl Alu {
    fn get_mut(&mut self, m: Mem) -> &mut i64 {
        self.memory.get_mut(&m).unwrap()
    }

    fn get(&mut self, m: Mem) -> i64 {
        *self.memory.get(&m).unwrap()
    }

    fn reset(&mut self) {
        self.memory.insert(Mem('w'), 0);
        self.memory.insert(Mem('x'), 0);
        self.memory.insert(Mem('y'), 0);
        self.memory.insert(Mem('z'), 0);
    }
}

impl FromStr for Alu {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut memory = HashMap::new();
        memory.insert(Mem('w'), 0);
        memory.insert(Mem('x'), 0);
        memory.insert(Mem('y'), 0);
        memory.insert(Mem('z'), 0);
        let instructions = s.lines().fold(Vec::new(), |mut acc, l| match &l[0..3] {
            "inp" => {
                let o = l.split(" ").skip(1).flat_map(|s| s.chars()).collect_vec();
                acc.push(Inst::new(Op::Inp, Mem(o[0]), None));
                acc
            }
            v => {
                let op = match v {
                    "add" => Op::Add,
                    "mul" => Op::Mul,
                    "div" => Op::Div,
                    "mod" => Op::Mod,
                    "eql" => Op::Eql,
                    _ => panic!("neineinei"),
                };
                let o = l.split(" ").skip(1).collect_vec();
                let rhs = match o[1].parse::<i64>() {
                    Ok(n) => Arg::Num(n as i64),
                    Err(_) => Arg::Mem(o[1].chars().nth(0).unwrap()),
                };
                acc.push(Inst::new(op, Mem(o[0].chars().nth(0).unwrap()), Some(rhs)));
                acc
            }
        });
        Ok(Self {
            memory,
            instructions,
        })
    }
}
