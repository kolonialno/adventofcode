use itertools::Itertools;
use std::{collections::HashMap, str::FromStr};

const ITERS: i32 = 50;
const DIRS: [(i32, i32); 9] = [
    (-1, -1),
    (0, -1),
    (1, -1),
    (-1, 0),
    (0, 0),
    (1, 0),
    (-1, 1),
    (0, 1),
    (1, 1),
];

fn main() {
    let (b_matcher, inp_img) = include_str!("../input.txt")
        .split("\n\n")
        .collect_tuple()
        .unwrap();
    let b_matcher = b_matcher.chars().collect_vec();

    let mut curr_img = inp_img.parse::<Image>().unwrap();
    for _ in 0..50 {
        let mut res_img = Image::default();
        for row in -ITERS * 2..=ITERS * 5 {
            for col in -ITERS * 2..=ITERS * 5 {
                res_img
                    .0
                    .insert((col, row), curr_img.next_pixel((col, row), &b_matcher));
            }
        }
        curr_img = res_img;
    }
    println!("res {}", curr_img.pixel_count())
}

#[derive(Default)]
struct Image(HashMap<(i32, i32), char>);

impl Image {
    fn next_pixel(&mut self, (col, row): (i32, i32), b_matcher: &Vec<char>) -> char {
        let mut r = String::new();
        for (dx, dy) in DIRS {
            match *self.0.entry((col + dx, row + dy)).or_insert('.') {
                '.' => &r.push_str("0"),
                '#' => &r.push_str("1"),
                _ => panic!("shouldn't be possible .."),
            };
        }
        b_matcher[usize::from_str_radix(r.as_str(), 2).unwrap()]
    }

    fn pixel_count(&self) -> usize {
        self.0
            .iter()
            .filter(|(&(col, row), &c)| {
                c == '#'
                    && col > -ITERS - 5
                    && row > -ITERS - 5
                    && col < 100 + ITERS + 5
                    && row < 100 + ITERS + 5
            })
            .count()
    }
}

impl FromStr for Image {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Image(s.lines().enumerate().fold(
            HashMap::new(),
            |mut acc, (row, line)| {
                for (col, c) in line.chars().enumerate() {
                    acc.insert((col as i32, row as i32), c);
                }
                acc
            },
        )))
    }
}
