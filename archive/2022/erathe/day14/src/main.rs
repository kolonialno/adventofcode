use itertools::Itertools;

const X: usize = 700;
const Y: i32 = 200;
const SX: i32 = 500;
const SY: i32 = 1;
const DIRS: [(i32, i32); 3] = [(0, 1), (-1, 1), (1, 1)];

fn main() {
    let input = include_str!("../input.txt");

    println!("part 1: {}", part1(input));
    println!("part 2: {}", part2(input));
}

fn part2(input: &str) -> usize {
    let (mut grid, max_y) = generate_grid(input);

    for c in grid.iter_mut().take(X) {
        c[(Y - (max_y + 2)) as usize] = '#';
    }

    'outer: loop {
        let (mut sx, mut sy) = (SX, SY);
        'inner: loop {
            for (dx, dy) in DIRS {
                if grid[(sx + dx) as usize][(Y - (sy + dy)) as usize] == '.' {
                    sx += dx;
                    sy += dy;
                    continue 'inner;
                }
            }
            // at the top, break
            if (Y - sy) == Y - SY {
                break 'outer;
            }
            // couldn't go in any direction. Place here and spawn new.
            grid[sx as usize][(Y - sy) as usize] = 'o';
            continue 'outer;
        }
    }

    grid.iter()
        .map(|row| row.iter().filter(|c| **c == 'o').count())
        .sum::<usize>()
}

fn part1(input: &str) -> usize {
    let (mut grid, max_y) = generate_grid(input);

    'outer: loop {
        let (mut sx, mut sy) = (SX, SY);
        'inner: loop {
            for (dx, dy) in DIRS {
                if grid[(sx + dx) as usize][(Y - (sy + dy)) as usize] == '.' {
                    sx += dx;
                    sy += dy;
                    // approaching inf
                    if sy > max_y {
                        break 'outer;
                    }
                    continue 'inner;
                }
            }
            // couldn't go in any direction. Place here and spawn new.
            grid[sx as usize][(Y - sy) as usize] = 'o';
            continue 'outer;
        }
    }

    grid.iter()
        .map(|row| row.iter().filter(|c| **c == 'o').count())
        .sum::<usize>()
}

fn generate_grid(input: &str) -> ([[char; 200]; X], i32) {
    let mut max_y = 0;
    let mut grid = [['.'; 200]; X];
    input.lines().for_each(|l| {
        let coords = l
            .split(" -> ")
            .map(|coord| {
                let (x, y) = coord.split_once(',').unwrap();
                (x.parse::<i32>().unwrap(), y.parse::<i32>().unwrap())
            })
            .collect_vec();
        for ((mut sx, mut sy), (dx, dy)) in coords.iter().zip(coords.clone().iter().skip(1)) {
            loop {
                grid[sx as usize][(Y - sy) as usize] = '#';
                if &sx == dx && &sy == dy {
                    break;
                }
                sx += (dx - sx).signum();
                sy += (dy - sy).signum();
                max_y = max_y.max(sy);
            }
        }
    });
    (grid, max_y)
}
