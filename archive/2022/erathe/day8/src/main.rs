use std::collections::HashMap;

const DIRS: [(i32, i32); 4] = [(-1, 0), (1, 0), (0, 1), (0, -1)];

fn main() {
    let input = include_str!("../input.txt");
    let forest = grow_forest(input);
    dbg!(solve(&forest));
}

fn solve(forest: &HashMap<(i32, i32), char>) -> (i32, i32) {
    let mut trees = 0;
    let mut res = 0;
    for ((x, y), size) in forest {
        let mut counted = false;
        let mut scenic = 1;
        'outer: for (dx, dy) in DIRS {
            let mut i = 1;
            while let Some(tree_in_way) = forest.get(&(x + (dx * i), y + (dy * i))) {
                if tree_in_way >= size {
                    scenic *= i;
                    continue 'outer;
                }
                i += 1;
            }
            if !counted {
                counted = true;
                trees += 1;
            }
            scenic *= i - 1;
        }
        res = res.max(scenic);
    }
    (trees, res)
}

fn grow_forest(seeds: &str) -> HashMap<(i32, i32), char> {
    let mut forest: HashMap<(i32, i32), char> = HashMap::new();
    for (y, line) in seeds.lines().enumerate() {
        for (x, seed) in line.chars().enumerate() {
            forest.insert((x as i32, y as i32), seed);
        }
    }
    forest
}
