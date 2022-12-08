#[derive(Debug)]
struct Grid {
    heights: Vec<u32>,
    stride: usize,
}

impl Grid {
    fn height(&self, row: usize, col: usize) -> u32 {
        self.heights[row * self.stride + col]
    }

    fn is_visible(&self, row: usize, col: usize) -> bool {
        // Trees on the edges are visible
        if row == 0 || row == self.stride - 1 || col == 0 || col == self.stride - 1 {
            return true;
        }

        let height_above = (0..row)
            .map(|r| self.height(r, col))
            .reduce(|highest, h| highest.max(h))
            .unwrap();
        let height_below = ((row + 1)..self.stride)
            .map(|r| self.height(r, col))
            .reduce(|highest, h| highest.max(h))
            .unwrap();
        let height_left = (0..col)
            .map(|c| self.height(row, c))
            .reduce(|highest, h| highest.max(h))
            .unwrap();
        let height_right = ((col + 1)..self.stride)
            .map(|c| self.height(row, c))
            .reduce(|highest, h| highest.max(h))
            .unwrap();

        let height = self.height(row, col);

        height > height_above
            || height > height_below
            || height > height_left
            || height > height_right
    }

    fn num_visible_trees(&self) -> usize {
        let mut count = 0;
        for row in 0..self.stride {
            for col in 0..self.stride {
                let visible = self.is_visible(row, col);
                if visible {
                    count += 1;
                }
            }
        }
        count
    }

    fn scenic_score(&self, row: usize, col: usize) -> usize {
        let height = self.height(row, col);

        let mut distance_above = 0;
        for r in (0..row).rev() {
            distance_above += 1;
            if self.height(r, col) >= height {
                break;
            }
        }

        let mut distance_below = 0;
        for r in (row + 1)..self.stride {
            distance_below += 1;
            if self.height(r, col) >= height {
                break;
            }
        }

        let mut distance_left = 0;
        for c in (0..col).rev() {
            distance_left += 1;
            if self.height(row, c) >= height {
                break;
            }
        }

        let mut distance_right = 0;
        for c in (col + 1)..self.stride {
            distance_right += 1;
            if self.height(row, c) >= height {
                break;
            }
        }

        distance_above * distance_below * distance_left * distance_right
    }

    fn max_scenic_score(&self) -> usize {
        let mut highest = 0;
        for row in 0..self.stride {
            for col in 0..self.stride {
                let scenic_score = self.scenic_score(row, col);
                if scenic_score > highest {
                    highest = scenic_score;
                }
            }
        }
        highest
    }
}

fn parse_grid(input: &str) -> Grid {
    let stride = input.lines().count();
    let heights: Vec<u32> = input
        .chars()
        .filter(|c| !c.is_whitespace())
        .map(|c| c.to_digit(10).expect("Input must be only numbers"))
        .collect();
    assert_eq!(heights.len() / stride, stride);
    Grid { heights, stride }
}

fn solve_part1(input: &str) -> usize {
    let grid = parse_grid(input);
    grid.num_visible_trees()
}

fn solve_part2(input: &str) -> usize {
    let grid = parse_grid(input);
    grid.max_scenic_score()
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
    fn test_grid() {
        let grid = parse_grid(include_str!("./test.txt"));
        assert!(grid.is_visible(0, 0), "0x0 should be visible");
        assert!(grid.is_visible(0, 1), "0x1 should be visible");
        assert!(grid.is_visible(0, 2), "0x2 should be visible");
        assert!(grid.is_visible(0, 3), "0x3 should be visible");
        assert!(grid.is_visible(0, 4), "0x4 should be visible");

        assert!(grid.is_visible(1, 0), "1x0 should be visible");
        assert!(grid.is_visible(1, 1), "1x1 should be visible");
        assert!(grid.is_visible(1, 2), "1x2 should be visible");
        assert!(!grid.is_visible(1, 3), "1x3 should be hidden");
        assert!(grid.is_visible(1, 4), "1x4 should be visible");

        assert!(grid.is_visible(2, 0), "2x0 should be visible");
        assert!(grid.is_visible(2, 1), "2x1 should be visible");
        assert!(!grid.is_visible(2, 2), "2x2 should be hidden");
        assert!(grid.is_visible(2, 3), "2x3 should be visible");
        assert!(grid.is_visible(2, 4), "2x4 should be visible");

        assert!(grid.is_visible(3, 0), "3x0 should be visible");
        assert!(!grid.is_visible(3, 1), "3x1 should be hidden");
        assert!(grid.is_visible(3, 2), "3x2 should be visible");
        assert!(!grid.is_visible(3, 3), "3x3 should be hidden");
        assert!(grid.is_visible(3, 4), "3x4 should be visible");

        assert!(grid.is_visible(4, 0), "4x0 should be visible");
        assert!(grid.is_visible(4, 1), "4x1 should be visible");
        assert!(grid.is_visible(4, 2), "4x2 should be visible");
        assert!(grid.is_visible(4, 3), "4x3 should be visible");
        assert!(grid.is_visible(4, 4), "4x4 should be visible");
    }

    #[test]
    fn test_part1() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part1(input), 21, "Wrong result for pt. 1");
    }

    #[test]
    fn test_scenic_score() {
        let grid = parse_grid(include_str!("./test.txt"));
        assert_eq!(grid.scenic_score(1, 2), 4);
        assert_eq!(grid.scenic_score(3, 2), 8);
    }

    #[test]
    fn test_part2() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part2(input), 8, "Wrong result for pt. 2");
    }
}
