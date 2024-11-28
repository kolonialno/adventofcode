use crate::day18::Num::I;
use crate::day18::Num::P;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};
use std::ops::Add;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Num {
    P(Vec<Num>),
    I(u32),
}

impl Num {
    fn get_from_pair(&mut self, index: usize) -> Option<&mut Num> {
        match self {
            P(pair) => Some(&mut pair[index]),
            _ => None,
        }
    }
}

impl Add<&Num> for &Num {
    type Output = Num;

    fn add(self, other: &Num) -> Num {
        let v1 = self.clone();
        let v2 = (*other).clone();
        let mut ans = P(vec![v1, v2]);
        let mut mutated = true;
        while mutated {
            mutated = reduce(&mut ans);
        }
        ans
    }
}

fn find_split(num: &str) -> usize {
    let mut depth = 0;
    num.chars()
        .position(|c| match c {
            '[' => {
                depth += 1;
                false
            }
            ']' => {
                depth -= 1;
                false
            }
            ',' => {
                if depth == 0 {
                    true
                } else {
                    false
                }
            }
            _ => false,
        })
        .unwrap()
}

fn parse_num(num: &str) -> Num {
    match &num[0..1] {
        "[" => {
            let split = find_split(&num[1..num.len() - 1]);
            P(vec![
                parse_num(&num[1..split + 1]),
                parse_num(&num[split + 2..num.len() - 1]),
            ])
        }
        _ => I(num[0..1].parse().unwrap()),
    }
}

fn read<R: Read>(io: R) -> Result<Vec<Num>, Error> {
    let mut nums: Vec<Num> = vec![];
    for line in BufReader::new(io).lines() {
        nums.push(parse_num(line.unwrap().trim()))
    }
    Ok(nums)
}

fn find_4_deep(num: &mut Num, mut loc: &mut Vec<usize>, depth: u8) -> bool {
    if depth == 4 {
        return true;
    }
    for i in 0..2 {
        match num.get_from_pair(i) {
            Some(mut sub_num) => {
                if let P(_pair) = sub_num {
                    loc.push(i);
                    if find_4_deep(&mut sub_num, &mut loc, depth + 1) {
                        return true;
                    }
                    loc.pop();
                }
            }
            None => {}
        }
    }
    false
}

fn loc_getter<'a>(mut num: &'a mut Num, loc: &Vec<usize>) -> &'a mut Num {
    for i in loc.iter() {
        num = num.get_from_pair(*i).unwrap()
    }
    num
}

fn find_augend(num: &mut Num, mut augend_loc: Vec<usize>, dir: usize, addend: u32) {
    augend_loc.push(dir);
    let mut added = false;
    while !added {
        if let I(augend) = loc_getter(num, &augend_loc) {
            *augend += addend;
            added = true;
        }
        augend_loc.push(1 - dir);
    }
}

fn move_num_from_pair(num: &mut Num, loc: &mut Vec<usize>, dir: usize) {
    if loc.iter().any(|l| *l == dir) {
        let (i, _item) = loc
            .iter()
            .enumerate()
            .rev()
            .find(|(_i, l)| **l == dir)
            .unwrap();
        loc.push(1 - dir);
        let addend = match loc_getter(num, &loc) {
            I(i) => *i,
            _ => panic!("Must be a number to be addable!"),
        };
        loc.pop();
        let augend_loc = Vec::from_iter(loc[0..i].iter().cloned());
        find_augend(num, augend_loc, 1 - dir, addend);
    }
}

fn explode(num: &mut Num) -> bool {
    let mut loc = vec![];
    if find_4_deep(num, &mut loc, 0) {
        move_num_from_pair(num, &mut loc, 0);
        move_num_from_pair(num, &mut loc, 1);
        let dying_pair = loc_getter(num, &loc);
        *dying_pair = I(0);
        true
    } else {
        false
    }
}

fn split(num: &mut Num) -> bool {
    for i in 0..2 {
        if let Some(sub_num) = num.get_from_pair(i) {
            match sub_num {
                I(i) => {
                    if *i > 9 {
                        *sub_num = P(vec![I(*i / 2), I((*i + 1) / 2)]);
                        return true;
                    }
                }
                P(_) => {
                    if split(sub_num) {
                        return true;
                    }
                }
            }
        }
    }
    false
}

fn reduce(num: &mut Num) -> bool {
    if explode(num) {
        return true;
    } else {
        split(num)
    }
}

fn magnitude(num: &Num) -> u32 {
    match num {
        P(pair) => 3 * magnitude(&pair[0]) + 2 * magnitude(&pair[1]),
        I(i) => *i,
    }
}

pub fn day18() {
    let nums = read(File::open("inputs/day18.txt").unwrap()).unwrap();
    let answer1 = nums[1..nums.len()]
        .iter()
        .fold(nums[0].clone(), |a, b| &a + b);
    let answer1 = magnitude(&answer1);
    println!("answer1 {:?}", answer1);

    let mut max_sum = 0;
    for i in 0..nums.len() {
        for ii in 0..nums.len() {
            max_sum = max_sum.max(magnitude(&(&nums[i] + &nums[ii])));
        }
    }
    println!("answer2 {:?}", max_sum);
}
