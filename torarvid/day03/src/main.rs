use std::format;
use std::fs;
use std::str::Lines;

fn main() {
    let filename = "input.txt";
    let contents =
        fs::read_to_string(filename).expect(&format!("Failed to read file {}", filename));
    println!("Part1: {}", solve(contents.lines(), 3, 1));
    println!("Part2: {}", solve_part2(contents));
}

fn solve(input: Lines, x_displacement: usize, y_displacement: usize) -> i32 {
    let mut pos_x = 0;
    let mut trees = 0;
    for (line_num, line) in input.enumerate().step_by(y_displacement) {
        let chars: Vec<char> = line.chars().collect();
        if line_num > 0 && chars[pos_x % chars.len()] == '#' {
            trees += 1;
        }
        pos_x += x_displacement % chars.len();
    }
    trees
}

fn solve_part2(input: String) -> i32 {
    let mut product = solve(input.lines(), 1, 1);
    product *= solve(input.lines(), 3, 1);
    product *= solve(input.lines(), 5, 1);
    product *= solve(input.lines(), 7, 1);
    product * solve(input.lines(), 1, 2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let test_data = String::from(
            "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#",
        );

        assert_eq!(solve(test_data.lines(), 1, 1), 2);
        assert_eq!(solve(test_data.lines(), 3, 1), 7);
        assert_eq!(solve(test_data.lines(), 5, 1), 3);
        assert_eq!(solve(test_data.lines(), 7, 1), 4);
        assert_eq!(solve(test_data.lines(), 1, 2), 2);
    }
}
