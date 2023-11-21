use std::{collections::HashSet, ops::RangeInclusive};

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
struct Point {
    x: usize,
    y: usize,
}

static SAND_ENTRYPOINT: Point = Point { x: 500, y: 0 };

impl From<&str> for Point {
    fn from(value: &str) -> Self {
        let (x, y) = value.split_once(",").expect("Invalid point");

        Point {
            x: x.parse().expect("Invalid x value"),
            y: y.parse().expect("Invalid y value"),
        }
    }
}

impl Point {
    fn below(&self) -> Point {
        Point {
            x: self.x,
            y: self.y + 1,
        }
    }

    fn left(&self) -> Point {
        Point {
            x: self.x - 1,
            y: self.y,
        }
    }

    fn right(&self) -> Point {
        Point {
            x: self.x + 1,
            y: self.y,
        }
    }
}

#[derive(Debug)]
enum Line {
    Horizontal(usize, RangeInclusive<usize>),
    Vertical(usize, RangeInclusive<usize>),
}

impl From<&[Point]> for Line {
    fn from(points: &[Point]) -> Self {
        let (a, b) = (points[0], points[1]);

        if a.x == b.x {
            Self::Vertical(a.x, if a.y < b.y { a.y..=b.y } else { b.y..=a.y })
        } else if a.y == b.y {
            Self::Horizontal(a.y, if a.x < b.x { a.x..=b.x } else { b.x..=a.x })
        } else {
            panic!("Invalid line")
        }
    }
}

impl Line {
    /// Check if the given point is on this line
    fn is_hit(&self, point: &Point) -> bool {
        match self {
            Self::Horizontal(y, x_range) => *y == point.y && x_range.contains(&point.x),
            Self::Vertical(x, y_range) => *x == point.x && y_range.contains(&point.y),
        }
    }

    fn start(&self) -> Point {
        match self {
            Self::Vertical(x, y_range) => Point {
                x: *x,
                y: *y_range.start(),
            },
            Self::Horizontal(y, x_range) => Point {
                x: *x_range.start(),
                y: *y,
            },
        }
    }

    fn stop(&self) -> Point {
        match self {
            Self::Vertical(x, y_range) => Point {
                x: *x,
                y: *y_range.end(),
            },
            Self::Horizontal(y, x_range) => Point {
                x: *x_range.end(),
                y: *y,
            },
        }
    }
}

#[derive(Debug)]
struct Path {
    lines: Vec<Line>,
}

impl From<&str> for Path {
    fn from(value: &str) -> Self {
        let points: Vec<Point> = value
            .split("->")
            .map(|coordinate| coordinate.trim().into())
            .collect();

        Path {
            lines: points.windows(2).map(|points| points.into()).collect(),
        }
    }
}

impl Path {
    /// Check if the given point is on this path
    fn is_hit(&self, point: &Point) -> bool {
        self.lines.iter().any(|line| line.is_hit(point))
    }

    fn is_below(&self, point: &Point) -> bool {
        self.lines.iter().any(|line| match line {
            Line::Vertical(x, y_range) => *x == point.x && *y_range.end() > point.y,
            Line::Horizontal(y, x_range) => *y > point.y && x_range.contains(&point.x),
        })
    }

    fn points(&self) -> Vec<Point> {
        let mut points: Vec<Point> = vec![self.lines[0].start()];
        for line in &self.lines {
            points.push(line.stop());
        }
        points
    }
}

#[derive(Debug)]
struct Scan {
    rocks: Vec<Path>,
    sand: HashSet<Point>,
    floor: Option<usize>,
}

impl From<&str> for Scan {
    fn from(input: &str) -> Self {
        let paths: Vec<Path> = input.lines().map(|line| line.into()).collect();
        Scan {
            rocks: paths,
            sand: HashSet::new(),
            floor: None,
        }
    }
}

impl Scan {
    /// Check if the given point is open
    fn is_taken(&self, point: &Point) -> bool {
        self.sand.contains(point)
            || self.rocks.iter().any(|path| path.is_hit(point))
            || (self.floor.is_some() && point.y >= self.floor.unwrap())
    }

    /// Check if the given point will result in a fall into the void
    fn is_void(&self, point: &Point) -> bool {
        self.floor.is_none() && !self.rocks.iter().any(|path| path.is_below(point))
    }

    fn find_floor(&self) -> usize {
        let mut y = 0;
        for path in &self.rocks {
            for point in &path.points() {
                if point.y > y {
                    y = point.y;
                }
            }
        }

        println!("Floor is at: {}", y + 2);

        y + 2
    }

    /// Drop the next unit of sand
    /// Returns false if it fell into the void
    fn drop_sand(&mut self, point: &Point) -> bool {
        if self.is_taken(point) || self.is_void(point) {
            return false;
        }

        let below = point.below();
        let below_left = below.left();
        let below_right = below.right();

        if !self.is_taken(&below) {
            return self.drop_sand(&below);
        } else if !self.is_taken(&below_left) {
            return self.drop_sand(&below_left);
        } else if !self.is_taken(&below_right) {
            return self.drop_sand(&below_right);
        }

        self.sand.insert(point.clone());
        true
    }

    fn extent(&self) -> (Point, Point) {
        let mut extent = (SAND_ENTRYPOINT, SAND_ENTRYPOINT);

        for path in &self.rocks {
            for point in &path.points() {
                if point.x < extent.0.x {
                    extent.0 = Point {
                        x: point.x,
                        y: extent.0.y,
                    };
                } else if point.x > extent.1.x {
                    extent.1 = Point {
                        x: point.x,
                        y: extent.1.y,
                    }
                }

                if point.y < extent.0.y {
                    extent.0 = Point {
                        x: extent.0.x,
                        y: point.y,
                    }
                } else if point.y > extent.1.y {
                    extent.1 = Point {
                        x: extent.1.x,
                        y: point.y,
                    }
                }
            }
        }

        extent
    }

    fn print(&self) {
        let extent = self.extent();
        for y in extent.0.y..=extent.1.y {
            for x in extent.0.x..=extent.1.x {
                let point = Point { x, y };

                if point == SAND_ENTRYPOINT {
                    print!("+");
                } else if self.sand.contains(&point) {
                    print!("o");
                } else if self.is_taken(&point) {
                    print!("#");
                } else {
                    print!(".");
                }
            }
            print!("\n");
        }
    }

    fn fill_with_sand(&mut self) {
        loop {
            if !self.drop_sand(&SAND_ENTRYPOINT) {
                break;
            }
        }
    }
}

fn solve_part1(input: &str) -> usize {
    let mut scan: Scan = input.into();
    scan.fill_with_sand();
    scan.print();
    scan.sand.len()
}

fn solve_part2(input: &str) -> usize {
    let mut scan: Scan = input.into();
    scan.floor = Some(scan.find_floor());
    scan.fill_with_sand();
    scan.print();
    scan.sand.len()
}

fn main() {
    let input = include_str!("./input.txt");
    let sum = solve_part1(input);
    println!("Part 1: {sum}");
    let sum = solve_part2(input);
    println!("Part 2: {sum}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path() {
        let path: Path = "0,1 -> 0,10".into();
        assert!(!path.is_hit(&Point { x: 0, y: 0 }));
        assert!(path.is_hit(&Point { x: 0, y: 1 }));
        assert!(path.is_hit(&Point { x: 0, y: 5 }));
        assert!(path.is_hit(&Point { x: 0, y: 10 }));
        assert!(!path.is_hit(&Point { x: 0, y: 11 }));
        assert!(!path.is_hit(&Point { x: 1, y: 1 }));
        assert!(!path.is_hit(&Point { x: 1, y: 0 }));

        let path: Path = "502,9 -> 494,9".into();
        assert!(!path.is_below(&Point { x: 493, y: 0 }));
        assert!(!path.is_below(&Point { x: 493, y: 8 }));
        assert!(!path.is_below(&Point { x: 493, y: 9 }));
        assert!(!path.is_below(&Point { x: 493, y: 10 }));
        assert!(path.is_below(&Point { x: 494, y: 0 }));
        assert!(path.is_below(&Point { x: 494, y: 8 }));
        assert!(!path.is_below(&Point { x: 494, y: 9 }));
        assert!(!path.is_below(&Point { x: 494, y: 10 }));
        assert!(path.is_below(&Point { x: 502, y: 0 }));
        assert!(path.is_below(&Point { x: 502, y: 8 }));
        assert!(!path.is_below(&Point { x: 502, y: 9 }));
        assert!(!path.is_below(&Point { x: 502, y: 10 }));
        assert!(!path.is_below(&Point { x: 503, y: 0 }));
        assert!(!path.is_below(&Point { x: 503, y: 8 }));
        assert!(!path.is_below(&Point { x: 503, y: 9 }));
        assert!(!path.is_below(&Point { x: 503, y: 10 }));

        let path: Path = "503,4 -> 502,4 -> 502,9 -> 494,9".into();
        assert!(!path.is_below(&Point { x: 493, y: 0 }));
        assert!(path.is_below(&Point { x: 494, y: 0 }));
        assert!(path.is_below(&Point { x: 494, y: 8 }));
        assert!(path.is_below(&Point { x: 500, y: 0 }));
        assert!(path.is_below(&Point { x: 500, y: 5 }));
        assert!(path.is_below(&Point { x: 500, y: 8 }));
        assert!(!path.is_below(&Point { x: 500, y: 9 }));
        assert!(path.is_below(&Point { x: 503, y: 0 }));
        assert!(path.is_below(&Point { x: 503, y: 3 }));
        assert!(!path.is_below(&Point { x: 503, y: 4 }));
    }

    #[test]
    fn test_part1() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part1(input), 24, "Wrong result for pt. 1");
    }

    #[test]
    fn test_part2() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part2(input), 93, "Wrong result for pt. 2");
    }
}
