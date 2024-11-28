use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    fs,
    ops::Add,
};

use helpers::get_input_file;
use itertools::Itertools;

const CHAMBER_WIDTH: isize = 7;

#[derive(Debug, Clone, Copy)]
enum Shape {
    HorLine,
    Plus,
    Corner,
    VerLine,
    Square,
}

impl Shape {
    fn occupied_positions(&self, pos: Position) -> Vec<Position> {
        let mut positions = self.occupied_bottom(pos);
        positions.extend(self.occupied_non_bottom(pos).iter());
        positions
    }

    fn occupied_non_bottom(&self, pos: Position) -> Vec<Position> {
        match self {
            Shape::HorLine => vec![],
            Shape::Plus => vec![
                pos + Position::right() + Position::up(),
                pos + Position::right() + Position::up().mul(2),
            ],
            Shape::Corner => vec![
                pos + Position::up() + Position::right().mul(2),
                pos + Position::up().mul(2) + Position::right().mul(2),
            ],
            Shape::VerLine => vec![
                pos + Position::up(),
                pos + Position::up().mul(2),
                pos + Position::up().mul(3),
            ],
            Shape::Square => vec![
                pos + Position::up(),
                pos + Position::up() + Position::right(),
            ],
        }
    }

    fn occupied_bottom(&self, pos: Position) -> Vec<Position> {
        match self {
            Shape::HorLine => vec![
                pos,
                pos + Position::right(),
                pos + Position::right().mul(2),
                pos + Position::right().mul(3),
            ],
            Shape::Plus => vec![
                pos + Position::right(),
                pos + Position::up(),
                pos + Position::right().mul(2) + Position::up(),
            ],
            Shape::Corner => vec![pos, pos + Position::right(), pos + Position::right().mul(2)],
            Shape::VerLine => vec![pos],
            Shape::Square => vec![pos, pos + Position::right()],
        }
    }

    fn highest_pos(&self, position: Position) -> isize {
        position.y
            + match self {
                Shape::HorLine => 0,
                Shape::Plus => 2,
                Shape::Corner => 2,
                Shape::VerLine => 3,
                Shape::Square => 1,
            }
    }
}

#[derive(Copy, Clone, Hash, Default, Eq, PartialEq, Debug)]
struct Position {
    x: isize,
    y: isize,
}

impl Position {
    fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }

    fn right() -> Self {
        Position::new(1, 0)
    }

    fn left() -> Self {
        Position::new(-1, 0)
    }

    fn down() -> Self {
        Position::new(0, -1)
    }

    fn up() -> Self {
        Position::new(0, 1)
    }

    fn mul(&self, factor: isize) -> Self {
        Position::new(factor * self.x, factor * self.y)
    }
}

impl Add for Position {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Position {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct Rock {
    shape: Shape,
    position: Position, // left-down position of 9 square grid
}

impl Rock {
    fn new(shape: Shape, position: Position) -> Self {
        Self { shape, position }
    }

    fn movement(&mut self, movement: Position, occupied: &HashSet<Position>) -> bool {
        let new_pos = self.position + movement;
        let occupied_bottom = self.shape.occupied_bottom(new_pos);
        let occupied_non_bottom = self.shape.occupied_non_bottom(new_pos);
        let mut settled = false;
        if !occupied_non_bottom
            .iter()
            .chain(occupied_bottom.iter())
            .any(|e| e.x < 0 || e.y < 0 || e.x >= CHAMBER_WIDTH || occupied.contains(e))
        {
            self.position = new_pos;
        } else if movement == Position::down()
            && occupied_bottom
                .iter()
                .any(|e| e.y < 0 || occupied.contains(e))
        {
            settled = true;
        }
        settled
    }
}

fn create_rock_sequence() -> [Shape; 5] {
    [
        Shape::HorLine,
        Shape::Plus,
        Shape::Corner,
        Shape::VerLine,
        Shape::Square,
    ]
}

fn print_grid(occupied: &HashSet<Position>) {
    let mut x = 0;
    let mut positions = occupied.iter().cloned().sorted_by_key(|e| (e.y, -e.x));
    let mut y = positions[positions.len() - 1].y;

    let mut lines = Vec::default();
    let mut line = String::with_capacity(CHAMBER_WIDTH as usize + 2);
    line.push('|');
    while !positions.is_empty() {
        let pos = positions.pop().unwrap();
        if pos.y != y {
            for _ in x..CHAMBER_WIDTH as usize {
                line.push('.');
            }
            line.push('|');
            lines.push(line);

            line = String::with_capacity(CHAMBER_WIDTH as usize + 2);
            line.push('|');
            y = pos.y;
            x = 0;
        }
        for _ in x..pos.x as usize {
            line.push('.');
        }
        line.push('#');
        x = pos.x as usize + 1;
    }
    if line.len() < CHAMBER_WIDTH as usize + 2 {
        for _ in x..CHAMBER_WIDTH as usize {
            line.push('.');
        }
        line.push('|');
        lines.push(line);
    }
    lines.push(String::from("+-------+"));
    for line in lines {
        println!("{}", line);
    }
}

type Pattern = [isize; CHAMBER_WIDTH as usize];

#[derive(Copy, Clone, Debug)]
struct PatternPos {
    earliest: usize,
    interval: usize,
    height_diff: usize,
}

fn settling_rocks(input: &str, nr_of_rocks: usize) -> usize {
    let mut patterns: HashMap<Pattern, Vec<(usize, isize)>> = HashMap::default();
    let mut current_peaks = [-1; CHAMBER_WIDTH as usize];
    let stream = input
        .chars()
        .filter(|c| *c == '<' || *c == '>')
        .map(|x| match x {
            '<' => Position::left(),
            '>' => Position::right(),
            _ => panic!("unknown jet stream"),
        })
        .collect::<Vec<Position>>();
    let mut stream_iter = stream.iter().cycle();
    let down_movement = Position::down();
    let rock_shapes = create_rock_sequence();
    let mut shape_iter = rock_shapes.iter().cycle();
    let mut stop_rock_pattern_mark: Option<PatternPos> = None;
    let mut computed_height_diff = 0;
    let mut highest_rock_pos = -1;
    let mut occupied = HashSet::default();
    let mut rock = Rock::new(*shape_iter.next().unwrap(), Position::new(2, 3));
    let mut stopped_rocks = 0;
    while stopped_rocks < nr_of_rocks {
        let mut settled = rock.movement(*stream_iter.next().unwrap(), &occupied);
        if !settled {
            settled = rock.movement(down_movement, &occupied);
        }
        if settled {
            if stopped_rocks == 1408 {
                println!("WHAT???");
            }
            stopped_rocks += 1;
            for pos in rock.shape.occupied_positions(rock.position) {
                current_peaks[pos.x as usize] = max(current_peaks[pos.x as usize], pos.y);
                occupied.insert(pos);
            }
            highest_rock_pos = max(highest_rock_pos, rock.shape.highest_pos(rock.position));

            // create pattern from peaks
            let lowest_peak = current_peaks.iter().min().unwrap();
            let mut new_pattern = [0; CHAMBER_WIDTH as usize];
            for (x, y) in current_peaks.iter().enumerate() {
                if lowest_peak.is_negative() {
                    new_pattern[x] = y + lowest_peak.abs();
                } else {
                    new_pattern[x] = y - lowest_peak;
                }
            }
            patterns
                .entry(new_pattern)
                .or_insert_with(Vec::default)
                .push((stopped_rocks, highest_rock_pos));

            // check
            if let Some(pattern_pos) = stop_rock_pattern_mark {
                if (stopped_rocks - pattern_pos.earliest) % pattern_pos.interval == 0 {
                    // directly at point for interval
                    let nr_intervals = (nr_of_rocks - stopped_rocks) / pattern_pos.interval;
                    stopped_rocks += nr_intervals * pattern_pos.interval;
                    computed_height_diff += nr_intervals * pattern_pos.height_diff;
                }
            }

            // print_grid(&occupied);
            rock = Rock::new(
                *shape_iter.next().unwrap(),
                Position::new(2, highest_rock_pos + 4),
            );

            if stop_rock_pattern_mark.is_none() && stopped_rocks % 10000 == 0 {
                // find patterns
                println!("Finding patterns after {} rocks stopped.", stopped_rocks);
                let mut min_diff = usize::MAX;
                let mut new_pattern: Option<PatternPos> = None;
                for (_, values) in patterns.iter() {
                    if values.len() >= 2 {
                        // repeating patterns
                        let mut diffs: HashMap<usize, Vec<(isize, usize, isize)>> =
                            HashMap::default();
                        for (idx, (stopped1, highest1)) in values.iter().enumerate() {
                            for (stopped2, highest2) in values.iter().skip(idx + 1) {
                                if stopped2 % stopped1 == 0 && highest2 % highest1 == 0 {
                                    let stopped_diff = stopped2 - stopped1;
                                    let highest_diff = highest2 - highest1;
                                    let vals =
                                        diffs.entry(stopped_diff).or_insert_with(Vec::default);
                                    vals.push((highest_diff, *stopped1, *highest1));
                                    vals.push((highest_diff, *stopped2, *highest2));
                                }
                            }
                        }
                        if let Some(&stop_diff) = diffs.keys().min() {
                            let vs = diffs.get_mut(&stop_diff).unwrap();
                            vs.sort_unstable_by_key(|x| x.1);
                            vs.dedup();

                            if stop_diff < min_diff {
                                new_pattern = Some(PatternPos {
                                    earliest: vs[0].1,
                                    interval: stop_diff,
                                    height_diff: vs[0].0 as usize,
                                });
                                min_diff = stop_diff;
                            }
                        }
                    }
                }
                stop_rock_pattern_mark = new_pattern;
                if let Some(p) = stop_rock_pattern_mark {
                    println!("Smallest pattern: {:?}", p);
                }
            }
        }
    }
    (highest_rock_pos as usize) + 1 + computed_height_diff
}

fn main() {
    let input = fs::read_to_string(get_input_file()).unwrap();
    let highest_rock_pos = settling_rocks(&input, 2022);
    println!(
        "Tower height after {} rocks settled: {}",
        2022, highest_rock_pos
    );
    let highest_rock_pos = settling_rocks(&input, 1000000000000);
    println!(
        "Tower height after {} rocks settled: {}",
        1_000_000_000_000u64, highest_rock_pos
    );
}

#[cfg(test)]
mod test {
    use std::{fs, path::PathBuf};

    use crate::settling_rocks;

    fn get_input() -> String {
        let mut test_file = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        test_file.push("test.txt");
        fs::read_to_string(test_file).unwrap()
    }

    #[test]
    fn part_1_example() {
        let settled_rocks = settling_rocks(&get_input(), 2022);
        assert_eq!(settled_rocks, 3068);
    }

    #[test]
    fn part_2_example() {
        let settled_rocks = settling_rocks(&get_input(), 1_000_000_000_000);
        assert_eq!(settled_rocks, 1514285714288);
    }
}
