use std::{collections::HashMap, hash::Hash};

struct Coordinate(usize, usize);

#[derive(Debug)]
struct Map {
    heights: Vec<u8>,
    width: usize,
    height: usize,
    start: (usize, usize),
    end: (usize, usize),
}

impl From<&str> for Map {
    fn from(value: &str) -> Self {
        let width = value
            .lines()
            .next()
            .expect("Expected at least one line")
            .len();
        let mut start = None;
        let mut end = None;
        let heights: Vec<u8> = value
            .chars()
            .filter(|c| !c.is_whitespace())
            .enumerate()
            .map(|(i, mut c)| {
                if c == 'S' {
                    start = Some(i);
                    c = 'a';
                } else if c == 'E' {
                    end = Some(i);
                    c = 'z';
                }

                c as u8 - 97
            })
            .collect();

        let start = start.unwrap();
        let end = end.unwrap();
        let height = heights.len() / width;

        Map {
            heights,
            width,
            height,
            start: (start % width, start / width),
            end: (end % width, end / width),
        }
    }
}

impl Map {
    fn h(&self, pos: (usize, usize)) -> usize {
        pos.0.abs_diff(self.end.0) + pos.1.abs_diff(self.end.1)
    }

    fn height(&self, pos: (usize, usize)) -> u8 {
        self.heights[pos.1 * self.width + pos.0]
    }

    fn print(&self) {
        for y in 0..self.height {
            for x in 0..self.width {
                print!("{}", (self.height((x, y)) + 97) as char);
            }
            print!("\n");
        }
    }

    fn neighbors(&self, pos: (usize, usize)) -> Vec<(usize, usize)> {
        let height = self.height(pos);
        let (x, y) = pos;
        let mut neighbors = vec![];
        if x > 0 {
            neighbors.push((x - 1, y));
        }
        if x < self.width - 1 {
            neighbors.push((x + 1, y));
        }
        if y > 0 {
            neighbors.push((x, y - 1));
        }
        if y < self.height - 1 {
            neighbors.push((x, y + 1));
        }

        // Only return neighbors where the height difference is 1 or less
        let neighbors = neighbors
            .into_iter()
            .filter(|n| {
                let n_height = self.height(*n);
                n_height < height || n_height - height <= 1
            })
            .collect();

        neighbors
    }

    fn reconstruct_path(
        &self,
        came_from: HashMap<(usize, usize), (usize, usize)>,
        mut current: (usize, usize),
    ) -> Vec<(usize, usize)> {
        let mut path = vec![current];
        while came_from.contains_key(&current) {
            current = came_from[&current];
            path.push(current);
        }
        return path;
    }

    fn depth_first(&self, parents: &Vec<(usize, usize)>) -> Option<Vec<(usize, usize)>> {
        let mut shortest: Option<Vec<(usize, usize)>> = None;
        let parent = *parents.last().unwrap();
        for neighbor in self.neighbors(parent) {
            // We've already been here
            if parents.contains(&neighbor) {
                continue;
            }

            let mut path = parents.clone();
            path.push(neighbor);

            if neighbor == self.end {
                return Some(path);
            }

            if let Some(path) = self.depth_first(&path) {
                if let Some(shortest_path) = shortest {
                    if shortest_path.len() > path.len() {
                        shortest = Some(path);
                    } else {
                        shortest = Some(shortest_path);
                    }
                } else {
                    shortest = Some(path);
                }
            }
        }

        shortest
    }

    // A* search path algorithm
    fn a_star(&self, start: (usize, usize)) -> Vec<(usize, usize)> {
        let mut open_set: Vec<(usize, usize)> = vec![start];

        let mut came_from: HashMap<(usize, usize), (usize, usize)> = HashMap::new();

        let mut g_scores: HashMap<(usize, usize), usize> = HashMap::new();
        g_scores.insert(start, 0);

        let mut f_scores: HashMap<(usize, usize), usize> = HashMap::new();
        f_scores.insert(start, self.h(start));

        while !open_set.is_empty() {
            let current = open_set.remove(0);
            if current == self.end {
                return self.reconstruct_path(came_from, current);
            }

            for neighbor in self.neighbors(current) {
                // Score of neighbors are always + 1 as edgest don't have weight
                let g_score = g_scores.get(&current).unwrap() + 1;
                if g_score < *g_scores.get(&neighbor).unwrap_or(&usize::MAX) {
                    came_from.insert(neighbor, current);
                    g_scores.insert(neighbor, g_score);
                    f_scores.insert(neighbor, g_score + self.h(neighbor));
                    if !open_set.contains(&neighbor) {
                        open_set.push(neighbor);
                    }
                }
            }

            // Sort open set so the cheapest node is the first one
            open_set.sort_by(|a, b| {
                f_scores
                    .get(a)
                    .unwrap_or(&usize::MAX)
                    .cmp(f_scores.get(b).unwrap_or(&usize::MAX))
            });
        }

        panic!("No possible paths")
    }

    fn dijkstra(&self, starts: Vec<(usize, usize)>) -> Vec<(usize, usize)> {
        let mut q: Vec<(usize, usize)> = starts.clone();
        let mut dist: HashMap<(usize, usize), usize> = HashMap::new();
        let mut prev: HashMap<(usize, usize), (usize, usize)> = HashMap::new();

        for x in 0..self.width {
            for y in 0..self.height {
                let v = (x, y);
                dist.insert(v, usize::MAX);
                q.push(v);
            }
        }
        for start in starts {
            dist.insert(start, 0);
        }

        while !q.is_empty() {
            // Sort q so the cheapest node is the first one
            q.sort_by(|a, b| dist[a].cmp(&dist[b]));
            let u = q.remove(0);

            if u == self.end {
                return self.reconstruct_path(prev, u);
            }

            for v in self.neighbors(u).into_iter().filter(|v| q.contains(v)) {
                let alt = dist[&u].checked_add(1).unwrap_or(usize::MAX);
                if alt < dist[&v] {
                    dist.insert(v, alt);
                    prev.insert(v, u);
                }
            }
        }

        panic!("No path")
    }
}

fn solve_part1(map: &Map) -> usize {
    let path = map.dijkstra(vec![map.start]);
    path.len() - 1
}

fn solve_part2(map: &Map) -> usize {
    let possible_starts: Vec<(usize, usize)> = map
        .heights
        .iter()
        .enumerate()
        .filter(|(_, height)| **height == 0)
        .map(|(i, _)| (i % map.width, i / map.width))
        .collect();
    println!("Possible starts: {}", possible_starts.len());

    let path = map.dijkstra(possible_starts);
    path.len() - 1
}

fn main() {
    let map: Map = include_str!("./input.txt").into();
    let sum = solve_part1(&map);
    println!("Part 1: {sum}");
    let sum = solve_part2(&map);
    println!("Part 2: {sum}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map() {
        let map: Map = include_str!("./test.txt").into();
        assert_eq!(map.height((0, 0)), 0);
        assert_eq!(map.height((0, 1)), 0);
        assert_eq!(map.height((0, 2)), 0);
        assert_eq!(map.height((0, 3)), 0);
        assert_eq!(map.height((0, 4)), 0);
        assert_eq!(map.height((1, 0)), 0);
        assert_eq!(map.height((1, 1)), 1);
        assert_eq!(map.height((1, 2)), 2);
        assert_eq!(map.height((1, 3)), 2);
        assert_eq!(map.height((1, 4)), 1);
    }

    #[test]
    fn test_part1() {
        let map: Map = include_str!("./test.txt").into();
        assert_eq!(solve_part1(&map), 31, "Wrong result for pt. 1");
    }

    #[test]
    fn test_part2() {
        let input = include_str!("./test.txt").into();
        assert_eq!(solve_part2(&input), 29, "Wrong result for pt. 2");
    }
}
