use itertools::Itertools;
use nom::{
    bytes::complete::tag, character::complete::digit1, combinator::map, multi::separated_list1,
    sequence::separated_pair, IResult,
};

fn parse(i: &str) -> IResult<&str, Vec<[usize; 2]>> {
    let parse_digit_pair = |(x, y): (&str, &str)| [x.parse().unwrap(), y.parse().unwrap()];
    let digit_pair = map(separated_pair(digit1, tag(","), digit1), parse_digit_pair);
    separated_list1(tag(" -> "), digit_pair)(i)
}

fn make_all_points(v: &[[usize; 2]]) -> Vec<[usize; 2]> {
    ((v[0][0].min(v[1][0]))..=(v[0][0].max(v[1][0])))
        .flat_map(|x| ((v[0][1].min(v[1][1]))..=(v[0][1].max(v[1][1]))).map(move |y| [x, y]))
        .collect()
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Cave {
    Air,
    Rock,
    Sand,
}

struct Map {
    elements: Vec<Cave>,
    width: usize,
}

impl Map {
    fn set(&mut self, x: usize, y: usize, item: Cave) {
        self.elements[x + y * self.width] = item;
    }
    fn get(&self, x: usize, y: usize) -> Cave {
        self.elements[x + y * self.width]
    }
}

fn main() {
    // Can I ping torvald using the Interno bot?
    let input = include_str!("input.txt");
    let sand_start = [500, 0];
    let rock_ranges: Vec<Vec<[usize; 2]>> = input.lines().map(|l| parse(l).unwrap().1).collect();
    let mut rocks: Vec<[usize; 2]> = rock_ranges
        .iter()
        .flat_map(|r| r[..].windows(2).map(make_all_points))
        .flatten()
        .unique()
        .collect();
    let bottom = dbg!(rocks.iter().map(|b| b[1]).max().unwrap() + 2);
    let mut cave = Map {
        width: 1000,
        elements: vec![Cave::Air; bottom * 1000 * 2],
    };
    for x in 0..1000 {
        rocks.push([x, bottom]);
    }
    for rock in rocks {
        cave.set(rock[0], rock[1], Cave::Rock)
    }

    let mut next_sand = sand_start;
    let mut cur_sand = [next_sand[0] + 1, next_sand[1] + 1];
    while cave.get(500, 0) == Cave::Air {
        while next_sand != cur_sand {
            cur_sand = next_sand;
            next_sand = if cave.get(cur_sand[0], cur_sand[1] + 1) == Cave::Air {
                [cur_sand[0], cur_sand[1] + 1]
            } else if cave.get(cur_sand[0] - 1, cur_sand[1] + 1) == Cave::Air {
                [cur_sand[0] - 1, cur_sand[1] + 1]
            } else if cave.get(cur_sand[0] + 1, cur_sand[1] + 1) == Cave::Air {
                [cur_sand[0] + 1, cur_sand[1] + 1]
            } else {
                cur_sand
            };
        }
        cave.set(next_sand[0], next_sand[1], Cave::Sand);
        next_sand = sand_start;
    }
    dbg!(cave.elements.iter().filter(|e| *e == &Cave::Sand).count());
}
