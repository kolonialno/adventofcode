use std::fmt::Display;
use std::fmt::Error;
use std::fs;

fn main() {
    let content = fs::read_to_string("input.txt").expect("Failed to read input.txt");
    // println!("Part1: {}", solve_part1(&content));
    println!("Part2: {}", solve_part2(&content));
}

#[derive(Clone, Debug)]
struct Cube {
    dimensions: usize,
    coords: Vec<i64>,
    active: bool,
}

impl Cube {
    fn neighbors(&self) -> Vec<Vec<i64>> {
        let mut result = Vec::new();
        for dim in 0..self.dimensions {
            for foo in self.coords[dim] - 1..=self.coords[dim] + 1 {}
        }
        result
    }
}

struct Game {
    dimensions: usize,
    cubes: Vec<Cube>,
}

impl Game {
    fn init(input: &str, dimensions: usize) -> Game {
        let mut cubes = Vec::new();
        for (line_num, line) in input.lines().enumerate() {
            for (ch_num, ch) in line.chars().enumerate() {
                let active = match ch {
                    '#' => true,
                    '.' => false,
                    _ => panic!("wtf?!"),
                };
                cubes.push(Cube {
                    dimensions,
                    coords: vec![ch_num as i64, line_num as i64, 0, 0],
                    active,
                });
            }
        }
        Game { dimensions, cubes }
    }

    fn boundaries(&self) -> Vec<(i64, i64)> {
        let mut bounds = Vec::new();
        for dim in 0..self.dimensions {
            let min = self.cubes.iter().map(|c| c.coords[dim]).min().unwrap();
            let max = self.cubes.iter().map(|c| c.coords[dim]).max().unwrap();
            bounds.push((min, max));
        }
        bounds
    }

    fn active_at(&self, x: i64, y: i64, z: i64) -> bool {
        self.get_cube(vec![x, y, z]).map_or(false, |c| c.active)
    }

    fn get_cube(&self, coords: Vec<i64>) -> Option<Cube> {
        self.cubes
            .iter()
            .find(|c| c.coords == coords)
            .map(|c| c.clone())
    }

    fn cycle(&mut self) {
        let mut next_cubes = Vec::new();
        self.cubes = next_cubes;
    }
}

impl Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), Error> {
        // let ((min_x, max_x), (min_y, max_y), (min_z, max_z)) = self.boundaries();
        // for z in min_z..=max_z {
        //     write!(f, "layer z={}\n", z)?;
        //     for y in min_y..=max_y {
        //         for x in min_x..=max_x {
        //             let ch = if self.active_at(x, y, z) { '#' } else { '.' };
        //             write!(f, "{}", ch);
        //         }
        //         write!(f, "\n")?;
        //     }
        //     write!(f, "\n")?;
        // }
        Ok(())
    }
}

#[derive(Clone, Debug)]
struct Cube3D {
    x: i64,
    y: i64,
    z: i64,
    active: bool,
}

impl Cube3D {
    fn neighbors(&self) -> Vec<(i64, i64, i64)> {
        let mut result = Vec::new();
        for x in self.x - 1..=self.x + 1 {
            for y in self.y - 1..=self.y + 1 {
                for z in self.z - 1..=self.z + 1 {
                    if x == self.x && y == self.y && z == self.z {
                        continue;
                    }
                    result.push((x, y, z))
                }
            }
        }
        result
    }
}

#[derive(Clone, Debug)]
struct Cube4D {
    x: i64,
    y: i64,
    z: i64,
    w: i64,
    active: bool,
}

impl Cube4D {
    fn neighbors(&self) -> Vec<(i64, i64, i64, i64)> {
        let mut result = Vec::new();
        for x in self.x - 1..=self.x + 1 {
            for y in self.y - 1..=self.y + 1 {
                for z in self.z - 1..=self.z + 1 {
                    for w in self.w - 1..=self.w + 1 {
                        if x == self.x && y == self.y && z == self.z && w == self.w {
                            continue;
                        }
                        result.push((x, y, z, w))
                    }
                }
            }
        }
        result
    }
}

struct Game3D {
    cubes: Vec<Cube3D>,
}

impl Game3D {
    fn init(input: &str) -> Game3D {
        let mut cubes = Vec::new();
        for (line_num, line) in input.lines().enumerate() {
            for (ch_num, ch) in line.chars().enumerate() {
                let active = match ch {
                    '#' => true,
                    '.' => false,
                    _ => panic!("wtf?!"),
                };
                cubes.push(Cube3D {
                    x: ch_num as i64,
                    y: line_num as i64,
                    z: 0,
                    active,
                });
            }
        }
        Game3D { cubes }
    }

    fn boundaries(&self) -> ((i64, i64), (i64, i64), (i64, i64)) {
        let min_x = self.cubes.iter().min_by_key(|c| c.x).unwrap().x;
        let max_x = self.cubes.iter().max_by_key(|c| c.x).unwrap().x;
        let min_y = self.cubes.iter().min_by_key(|c| c.y).unwrap().y;
        let max_y = self.cubes.iter().max_by_key(|c| c.y).unwrap().y;
        let min_z = self.cubes.iter().min_by_key(|c| c.z).unwrap().z;
        let max_z = self.cubes.iter().max_by_key(|c| c.z).unwrap().z;
        ((min_x, max_x), (min_y, max_y), (min_z, max_z))
    }

    fn active_at(&self, x: i64, y: i64, z: i64) -> bool {
        match self.get_cube(x, y, z) {
            Some(cube) => cube.active,
            None => false,
        }
    }

    fn get_cube(&self, x: i64, y: i64, z: i64) -> Option<Cube3D> {
        self.cubes
            .iter()
            .find(|c| c.x == x && c.y == y && c.z == z)
            .map(|c| c.clone())
    }

    fn cycle(&mut self) {
        let mut next_cubes = Vec::new();
        let ((min_x, max_x), (min_y, max_y), (min_z, max_z)) = self.boundaries();
        for z in min_z - 1..=max_z + 1 {
            for y in min_y - 1..=max_y + 1 {
                for x in min_x - 1..=max_x + 1 {
                    let cube = self.get_cube(x, y, z).unwrap_or(Cube3D {
                        x,
                        y,
                        z,
                        active: false,
                    });
                    // println!("  X {:?} {:?}", cube, cube.neighbors());
                    let active_neighbors = cube
                        .neighbors()
                        .iter()
                        .map(|(x, y, z)| self.get_cube(*x, *y, *z))
                        .flatten()
                        .filter(|c| c.active)
                        .collect::<Vec<Cube3D>>();
                    // println!("$$ {:2} {:2} {:2} $$", x, y, z);
                    for c in &active_neighbors {
                        // println!("V {:?}", c);
                    }
                    let next_active = if cube.active {
                        active_neighbors.len() == 2 || active_neighbors.len() == 3
                    } else {
                        active_neighbors.len() == 3
                    };
                    next_cubes.push(Cube3D {
                        x: cube.x,
                        y: cube.y,
                        z: cube.z,
                        active: next_active,
                    });
                }
            }
        }
        self.cubes = next_cubes;
    }
}

impl Display for Game3D {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), Error> {
        let ((min_x, max_x), (min_y, max_y), (min_z, max_z)) = self.boundaries();
        for z in min_z..=max_z {
            write!(f, "layer z={}\n", z)?;
            for y in min_y..=max_y {
                for x in min_x..=max_x {
                    let ch = if self.active_at(x, y, z) { '#' } else { '.' };
                    write!(f, "{}", ch);
                }
                write!(f, "\n")?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

fn solve_part1(input: &str) -> usize {
    let mut game = Game3D::init(input);
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cubes.iter().filter(|&c| c.active).count()
}

struct Game4D {
    cubes: Vec<Cube4D>,
}

impl Game4D {
    fn init(input: &str) -> Game4D {
        let mut cubes = Vec::new();
        for (line_num, line) in input.lines().enumerate() {
            for (ch_num, ch) in line.chars().enumerate() {
                let active = match ch {
                    '#' => true,
                    '.' => false,
                    _ => panic!("wtf?!"),
                };
                cubes.push(Cube4D {
                    x: ch_num as i64,
                    y: line_num as i64,
                    z: 0,
                    w: 0,
                    active,
                });
            }
        }
        Game4D { cubes }
    }

    fn boundaries(&self) -> ((i64, i64), (i64, i64), (i64, i64), (i64, i64)) {
        let min_x = self.cubes.iter().min_by_key(|c| c.x).unwrap().x;
        let max_x = self.cubes.iter().max_by_key(|c| c.x).unwrap().x;
        let min_y = self.cubes.iter().min_by_key(|c| c.y).unwrap().y;
        let max_y = self.cubes.iter().max_by_key(|c| c.y).unwrap().y;
        let min_z = self.cubes.iter().min_by_key(|c| c.z).unwrap().z;
        let max_z = self.cubes.iter().max_by_key(|c| c.z).unwrap().z;
        let min_w = self.cubes.iter().min_by_key(|c| c.w).unwrap().w;
        let max_w = self.cubes.iter().max_by_key(|c| c.w).unwrap().w;
        (
            (min_x, max_x),
            (min_y, max_y),
            (min_z, max_z),
            (min_w, max_w),
        )
    }

    fn active_at(&self, x: i64, y: i64, z: i64, w: i64) -> bool {
        match self.get_cube(x, y, z, w) {
            Some(cube) => cube.active,
            None => false,
        }
    }

    fn get_cube(&self, x: i64, y: i64, z: i64, w: i64) -> Option<Cube4D> {
        self.cubes
            .iter()
            .find(|c| c.x == x && c.y == y && c.z == z && c.w == w)
            .map(|c| c.clone())
    }

    fn cycle(&mut self) {
        let mut next_cubes = Vec::new();
        let ((min_x, max_x), (min_y, max_y), (min_z, max_z), (min_w, max_w)) = self.boundaries();
        for z in min_z - 1..=max_z + 1 {
            for y in min_y - 1..=max_y + 1 {
                for x in min_x - 1..=max_x + 1 {
                    for w in min_w - 1..=max_w + 1 {
                        let cube = self.get_cube(x, y, z, w).unwrap_or(Cube4D {
                            x,
                            y,
                            z,
                            w,
                            active: false,
                        });
                        // println!("  X {:?} {:?}", cube, cube.neighbors());
                        let active_neighbors = cube
                            .neighbors()
                            .iter()
                            .map(|(x, y, z, w)| self.get_cube(*x, *y, *z, *w))
                            .flatten()
                            .filter(|c| c.active)
                            .collect::<Vec<Cube4D>>();
                        // println!("$$ {:2} {:2} {:2} $$", x, y, z);
                        for c in &active_neighbors {
                            // println!("V {:?}", c);
                        }
                        let next_active = if cube.active {
                            active_neighbors.len() == 2 || active_neighbors.len() == 3
                        } else {
                            active_neighbors.len() == 3
                        };
                        next_cubes.push(Cube4D {
                            x: cube.x,
                            y: cube.y,
                            z: cube.z,
                            w: cube.w,
                            active: next_active,
                        });
                    }
                }
            }
        }
        self.cubes = next_cubes;
    }
}

impl Display for Game4D {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), Error> {
        let ((min_x, max_x), (min_y, max_y), (min_z, max_z), (min_w, max_w)) = self.boundaries();
        for z in min_z..=max_z {
            write!(f, "layer z={}\n", z)?;
            for y in min_y..=max_y {
                for x in min_x..=max_x {
                    for w in min_w..=max_w {
                        let ch = if self.active_at(x, y, z, w) { '#' } else { '.' };
                        write!(f, "{}", ch)?;
                    }
                    write!(f, "\n")?;
                }
                write!(f, "\n")?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

fn solve_part2(input: &str) -> usize {
    let mut game = Game4D::init(input);
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cycle();
    println!("Game {}", game);
    game.cubes.iter().filter(|&c| c.active).count()
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn test_solve() {
    //     let test_data = ".#.\n..#\n###";
    //     assert_eq!(solve_part1(test_data), 112);
    //     assert_eq!(solve_part2(test_data), 848);
    // }

    #[test]
    fn test_perm() {
        let vec = vec![(2, 4), (5, 8), (1, 3)];
        let dims = vec.len();
        for d in 0..dims {}
    }
}
