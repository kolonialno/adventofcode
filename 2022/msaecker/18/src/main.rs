use std::{
    collections::{HashMap, HashSet},
    fs,
    ops::Add,
    str::FromStr,
};

use helpers::get_input_file;

#[derive(Copy, Clone, Hash, Default, Eq, PartialEq, Debug)]
struct Position {
    x: isize,
    y: isize,
    z: isize,
}

impl Position {
    fn new(x: isize, y: isize, z: isize) -> Self {
        Self { x, y, z }
    }

    fn get_sides(&self) -> Vec<Position> {
        vec![
            *self + Position::new(5, 0, 0),
            *self + Position::new(0, 5, 0),
            *self + Position::new(0, 0, 5),
            *self + Position::new(-5, 0, 0),
            *self + Position::new(0, -5, 0),
            *self + Position::new(0, 0, -5),
        ]
    }

    fn mul(&mut self, factor: isize) {
        self.x *= factor;
        self.y *= factor;
        self.z *= factor;
    }

    fn neighbors(&self) -> Vec<Position> {
        vec![
            *self + Position::new(1, 0, 0),
            *self + Position::new(0, 1, 0),
            *self + Position::new(0, 0, 1),
            *self + Position::new(-1, 0, 0),
            *self + Position::new(0, -1, 0),
            *self + Position::new(0, 0, -1),
        ]
    }
}

impl Add for Position {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Position {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl FromStr for Position {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let coords = s
            .split(',')
            .map(|i| i.parse::<isize>().unwrap())
            .collect::<Vec<isize>>();
        Ok(Position::new(coords[0], coords[1], coords[2]))
    }
}

fn get_cubes(input: &str) -> HashSet<Position> {
    input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| Position::from_str(line).unwrap())
        .collect::<HashSet<Position>>()
}

fn open_sides(input: &str) -> Vec<Position> {
    let mut sides: Vec<Position> = Vec::default();
    let cubes = get_cubes(input);
    for mut cube in cubes {
        cube.mul(10);
        sides.extend(cube.get_sides().iter());
    }
    sides.sort_by_key(|e| (e.x, e.y, e.z));
    let mut unique: HashMap<Position, usize> = HashMap::default();
    let mut remove_indexes: HashSet<usize> = HashSet::default();
    for (idx, side) in sides.iter().enumerate() {
        if unique.contains_key(side) {
            remove_indexes.insert(unique[side]);
            remove_indexes.insert(idx);
        } else {
            unique.insert(*side, idx);
        }
    }
    sides
        .into_iter()
        .enumerate()
        .filter(|(idx, _)| !remove_indexes.contains(idx))
        .map(|(_, side)| side)
        .collect::<Vec<Position>>()
}

fn surface(input: &str) -> usize {
    let cubes = get_cubes(input);
    let max_x = cubes.iter().map(|p| p.x).max().unwrap() + 1;
    let min_x = cubes.iter().map(|p| p.x).min().unwrap() - 1;
    let max_y = cubes.iter().map(|p| p.y).max().unwrap() + 1;
    let min_y = cubes.iter().map(|p| p.y).min().unwrap() - 1;
    let max_z = cubes.iter().map(|p| p.z).max().unwrap() + 1;
    let min_z = cubes.iter().map(|p| p.z).min().unwrap() - 1;

    let x_range = min_x..=max_x;
    let y_range = min_y..=max_y;
    let z_range = min_z..=max_z;

    let mut visited: HashSet<Position> = HashSet::default();
    let mut to_visit: Vec<Position> = Vec::default();
    let mut surface = 0;
    to_visit.push(Position::new(min_x, min_y, min_z));
    while !to_visit.is_empty() {
        let pos = to_visit.pop().unwrap();
        if visited.contains(&pos) {
            continue;
        }
        visited.insert(pos);
        for n in pos.neighbors() {
            if x_range.contains(&n.x) && y_range.contains(&n.y) && z_range.contains(&n.z) {
                if cubes.contains(&n) {
                    surface += 1;
                } else if !visited.contains(&n) {
                    to_visit.push(n);
                }
            }
        }
    }
    surface
}

fn main() {
    let input = fs::read_to_string(get_input_file()).unwrap();
    println!("Open sides: {}", open_sides(&input).len());
    println!("Surface sides: {}", surface(&input));
}

#[cfg(test)]
mod test {
    use std::{fs, path::PathBuf};

    use crate::{open_sides, surface};

    fn get_input() -> String {
        let mut test_file = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        test_file.push("test.txt");
        fs::read_to_string(test_file).unwrap()
    }

    #[test]
    fn part_1_example() {
        let open_sides = open_sides(&get_input());
        assert_eq!(open_sides.len(), 64);
    }

    #[test]
    fn part_2_example() {
        let open_sides = surface(&get_input());
        assert_eq!(open_sides, 58);
    }
}
