use itertools::Itertools;
use std::{
    collections::HashMap,
    ops::{Add, AddAssign, Sub},
    str::FromStr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Pos(i32, i32);

impl Pos {
    /// Prioritized vec of fall positions
    fn falls_to(&self) -> Vec<Pos> {
        vec![
            *self + Vec2D(0, 1),
            *self + Vec2D(-1, 1),
            *self + Vec2D(1, 1),
        ]
    }
}

impl Add<Vec2D> for Pos {
    type Output = Self;

    fn add(self, rhs: Vec2D) -> Self::Output {
        Pos(self.0 + rhs.0, self.1 + rhs.1)
    }
}

impl Sub for Pos {
    type Output = Vec2D;

    fn sub(self, rhs: Self) -> Self::Output {
        Vec2D(self.0 - rhs.0, self.1 - rhs.1)
    }
}

impl AddAssign<Vec2D> for Pos {
    fn add_assign(&mut self, rhs: Vec2D) {
        self.0 += rhs.0;
        self.1 += rhs.1;
    }
}

#[derive(Debug, Clone, Copy)]
struct Vec2D(i32, i32);

impl Vec2D {
    fn normalize(&self) -> Self {
        Vec2D(self.0 / self.0.abs().max(1), self.1 / self.1.abs().max(1))
    }
}

#[derive(Debug)]
struct Path(Vec<PathSegment>);

#[derive(Debug)]
struct PathSegment {
    start: Pos,
    end: Pos,
    step: Vec2D,
}

impl PathSegment {
    fn new(start: Pos, end: Pos) -> Self {
        Self {
            start,
            end,
            step: (end - start).normalize(),
        }
    }

    fn expand_positions(&self) -> Vec<Pos> {
        let mut pos = self.start;
        let mut positions = vec![pos];

        while pos != self.end {
            pos += self.step;
            positions.push(pos);
        }

        positions
    }
}

impl Path {
    fn expand_positions(&self) -> Vec<Pos> {
        self.0
            .iter()
            .flat_map(|segment| segment.expand_positions())
            .dedup()
            .collect_vec()
    }
}

#[derive(Debug, Clone, Copy)]
struct Bounds {
    left: i32,
    right: i32,
    top: i32,
    bottom: i32,
}

impl Bounds {
    fn from_pos(pos: &Pos) -> Self {
        Bounds {
            left: pos.0,
            right: pos.0,
            top: pos.1,
            bottom: pos.1,
        }
    }

    fn from_positions(iter: &mut dyn Iterator<Item = &Pos>) -> Self {
        let mut bounds = Bounds::from_pos(iter.next().unwrap());

        for pos in iter {
            bounds.expand_to_contain(pos);
        }

        bounds
    }

    fn expand_to_contain(&mut self, pos: &Pos) {
        self.left = self.left.min(pos.0);
        self.right = self.right.max(pos.0);
        self.top = self.top.min(pos.1);
        self.bottom = self.bottom.max(pos.1);
    }

    fn expand_down(&mut self, steps: i32) {
        self.bottom += steps;
    }

    fn is_in_void(&self, pos: &Pos) -> bool {
        pos.1 > self.bottom
    }
}

#[derive(Debug)]
enum HardObject {
    Rock,
    Sand,
}

#[derive(Debug)]
struct Cave {
    bounds: Bounds,
    occupation: HashMap<Pos, HardObject>,
    spawn_point: Pos,
    floor_y: Option<i32>,
}

impl Cave {
    fn from_paths(paths: Vec<Path>) -> Self {
        // Build an occupation matrix to collision test against.
        let mut occupation = HashMap::new();
        for path in paths {
            for pos in path.expand_positions() {
                occupation.insert(pos, HardObject::Rock);
            }
        }

        let spawn_point = Pos(500, 0);

        // Create bounds, encompassing all occupied positions as well as the
        // spawn point.
        let mut bounds = Bounds::from_positions(&mut occupation.keys());
        bounds.expand_to_contain(&spawn_point);

        Self {
            bounds,
            occupation,
            spawn_point,
            floor_y: None,
        }
    }

    fn is_floor(&self, pos: &Pos) -> bool {
        if let Some(floor_y) = self.floor_y {
            pos.1 == floor_y
        } else {
            false
        }
    }

    fn is_occupied(&self, pos: &Pos) -> bool {
        self.occupation.contains_key(pos) || self.is_floor(pos)
    }

    /// Spawns a unit of sand, returning the position where it comes to rest.
    fn spawn_sand(&self) -> Option<Pos> {
        let mut position = self.spawn_point;

        while !self.bounds.is_in_void(&position) {
            let maybe_next_position = position
                .falls_to()
                .into_iter()
                .find(|pos| !self.is_occupied(pos));

            if let Some(next_position) = maybe_next_position {
                position = next_position;
            } else if position != self.spawn_point {
                return Some(position);
            } else {
                return None;
            }
        }

        None
    }

    fn spawn_sand_until_stagnation(&mut self) -> usize {
        let mut num_resting_sand = 0;
        while let Some(sand_pos) = self.spawn_sand() {
            self.occupation.insert(sand_pos, HardObject::Sand);
            num_resting_sand += 1;
        }
        num_resting_sand
    }
}

fn run_part_one(s: &str) -> usize {
    let mut cave = s.parse::<Cave>().unwrap();
    cave.spawn_sand_until_stagnation()
}

fn run_part_two(s: &str) -> usize {
    let mut cave = s.parse::<Cave>().unwrap();

    // Expand the cave bounds and add a floor.
    cave.bounds.expand_down(2);
    cave.floor_y = Some(cave.bounds.bottom);

    cave.spawn_sand_until_stagnation() + 1
}

fn main() {
    let input = include_str!("../input.txt");

    let part_one = run_part_one(input);
    println!("Part 1: {part_one}");

    let part_two = run_part_two(input);
    println!("Part 2: {part_two}");
}

#[cfg(test)]
mod tests {
    use crate::{run_part_one, run_part_two, Path, PathSegment, Pos};

    static SAMPLE: &str = include_str!("../sample.txt");

    #[test]
    fn path_expansion() {
        let seg1 = PathSegment::new(Pos(498, 4), Pos(498, 6));
        let seg2 = PathSegment::new(Pos(498, 6), Pos(496, 6));

        assert_eq!(
            seg1.expand_positions(),
            vec![Pos(498, 4), Pos(498, 5), Pos(498, 6)]
        );
        assert_eq!(
            seg2.expand_positions(),
            vec![Pos(498, 6), Pos(497, 6), Pos(496, 6)]
        );

        let path = Path(vec![seg1, seg2]);
        assert_eq!(
            path.expand_positions(),
            vec![
                Pos(498, 4),
                Pos(498, 5),
                Pos(498, 6),
                Pos(497, 6),
                Pos(496, 6)
            ]
        );
    }

    #[test]
    fn sample_part_one_is_correct() {
        assert_eq!(run_part_one(SAMPLE), 24);
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two(SAMPLE), 93);
    }
}

impl FromStr for Cave {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let paths = s.lines().map(|line| line.parse::<Path>().unwrap());
        Ok(Self::from_paths(paths.collect_vec()))
    }
}

impl FromStr for Path {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let segments = s
            .split(" -> ")
            .map(|pos_str| pos_str.parse::<Pos>().unwrap())
            .tuple_windows()
            .map(|(start, end)| PathSegment::new(start, end))
            .collect_vec();

        Ok(Self(segments))
    }
}

impl FromStr for Pos {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x, y) = s.split_once(',').unwrap();
        Ok(Self(x.parse().unwrap(), y.parse().unwrap()))
    }
}
