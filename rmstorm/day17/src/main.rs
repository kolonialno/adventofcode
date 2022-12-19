use core::fmt;
use std::{collections::VecDeque, iter::Cycle};

#[derive(Debug)]
enum Shape {
    Angle,
    Block,
    Hbar,
    Plus,
    Vbar,
}
impl Shape {
    fn valid_pos(&self) -> Vec<isize> {
        match self {
            Shape::Hbar => vec![0, 1, 2, 3],
            Shape::Plus => vec![0, 1, 2, 3, 4],
            Shape::Angle => vec![0, 1, 2, 3, 4],
            Shape::Vbar => vec![0, 1, 2, 3, 4, 5, 6],
            Shape::Block => vec![0, 1, 2, 3, 4, 5],
        }
    }
    fn shape(&self) -> Vec<[usize; 2]> {
        match self {
            Shape::Hbar => vec![[0, 0], [1, 0], [2, 0], [3, 0]],
            Shape::Plus => vec![[0, 1], [1, 1], [2, 1], [1, 0], [1, 2]],
            Shape::Angle => vec![[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]],
            Shape::Vbar => vec![[0, 0], [0, 1], [0, 2], [0, 3]],
            Shape::Block => vec![[0, 0], [1, 0], [0, 1], [1, 1]],
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Fill {
    Air,
    Rock,
}

struct Cave {
    scrolled: i128,
    width: usize,
    elements: VecDeque<Fill>,
    jets: Cycle<std::slice::Iter<'static, u8>>,
}
impl Cave {
    fn add_shape(&mut self, shape: &Shape, coord: [usize; 2]) {
        for c in shape.shape() {
            self.set(Fill::Rock, [c[0] + coord[0], c[1] + coord[1]]);
        }
    }
    fn collision_on_coord(&self, shape: &Shape, coord: &[isize; 2]) -> bool {
        if coord[1] == -1 {
            return true;
        }
        for spot in shape.shape() {
            if spot[1] as isize + coord[1] == -1 {
                return true;
            }
            let spot: [usize; 2] = [
                spot[0] as usize + coord[0] as usize,
                spot[1] as usize + coord[1] as usize,
            ];
            if self.elements[spot[0] + self.width as usize * spot[1]] == Fill::Rock {
                return true;
            }
        }
        false
    }
    fn highest(&self) -> isize {
        match self.elements.iter().rev().position(|e| e == &Fill::Rock) {
            Some(r) => ((self.elements.len() / self.width) - r / self.width)
                .try_into()
                .unwrap(),
            None => 0,
        }
    }
    fn new(jets: &'static str) -> Cave {
        Cave {
            scrolled: 0,
            width: 7,
            elements: VecDeque::from(vec![Fill::Air; 10000 * 7]),
            jets: jets.as_bytes().iter().cycle(),
        }
    }
    fn set(&mut self, fill: Fill, coord: [usize; 2]) {
        self.elements[coord[1] * self.width as usize + coord[0]] = fill;
    }
    fn simulate_shape(&mut self, shape: &Shape) {
        let mut shape_pos = [2, self.highest() + 3];
        while !self.collision_on_coord(&shape, &shape_pos) {
            let potential_new_x = match self.jets.next() {
                Some(62) => shape_pos[0] + 1,
                Some(60) => shape_pos[0] - 1,
                _ => panic!("This should never happen"),
            } as isize;
            if shape.valid_pos().contains(&potential_new_x)
                && !self.collision_on_coord(&shape, &[potential_new_x, shape_pos[1]])
            {
                shape_pos[0] = potential_new_x;
            }
            shape_pos[1] -= 1;
        }
        self.add_shape(shape, [shape_pos[0] as usize, (shape_pos[1] + 1) as usize]);
        if self.highest() > 9900 {
            for i in 0..(self.width * 100) {
                self.elements.pop_front();
                self.elements.push_back(Fill::Air);
            }
            self.scrolled += 100;
            if self.scrolled % 1000000000 == 0 {
                dbg!(self.scrolled);
            }
        }
    }
}

// impl fmt::Debug for Cave {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         for row in self.elements.as_slices().0.chunks(7).rev() {
//             write!(f, "\n").unwrap();
//             for x in 0..self.width {
//                 match row[x] {
//                     Fill::Air => write!(f, ".").unwrap(),
//                     Fill::Rock => write!(f, "#").unwrap(),
//                 };
//             }
//         }
//         write!(f, "\n")
//     }
// }

fn main() {
    let test_input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";
    let input = include_str!("input.txt");
    let mut cave = Cave::new(test_input);
    let mut sequence = [
        Shape::Hbar,
        Shape::Plus,
        Shape::Angle,
        Shape::Vbar,
        Shape::Block,
    ]
    .iter()
    .cycle();
    for i in 0..(1000000000000 as u128) {
        cave.simulate_shape(sequence.next().unwrap());
    }
    dbg!(cave.highest() as i128 + cave.scrolled);
}
