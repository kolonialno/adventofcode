use std::format;
use std::fs;

fn main() {
    let filename = "input.txt";
    let contents =
        fs::read_to_string(filename).expect(&format!("Failed to read file {}", filename));
    let lines = contents.split('\n').collect();
    println!("Part1: {}", solve_part1(&lines));
    println!("Part2: {}", solve_part2(&lines));
}

fn solve_part1(input: &Vec<&str>) -> i32 {
    for (i, xs) in input.iter().enumerate() {
        for ys in &input[i + 1..] {
            if xs.chars().count() == 0 || ys.chars().count() == 0 {
                continue;
            }
            let x = xs.parse::<i32>().unwrap();
            let y = ys.parse::<i32>().unwrap();
            if x + y == 2020 {
                return x * y;
            }
        }
    }
    -1
}

fn solve_part2(input: &Vec<&str>) -> i32 {
    for (i, xs) in input.iter().enumerate() {
        for (j, ys) in input[i + 1..].iter().enumerate() {
            for zs in &input[j + 1..] {
                if xs.chars().count() == 0 || ys.chars().count() == 0 || zs.chars().count() == 0 {
                    continue;
                }
                let x = xs.parse::<i32>().unwrap();
                let y = ys.parse::<i32>().unwrap();
                let z = zs.parse::<i32>().unwrap();
                if x + y + z == 2020 {
                    return x * y * z;
                }
            }
        }
    }
    -1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_part1() {
        assert_eq!(
            solve_part1(
                &String::from("1721\n979\n366\n299\n675\n1456\n")
                    .split("\n")
                    .collect()
            ),
            514579,
        );
    }

    #[test]
    fn test_solve_part2() {
        assert_eq!(
            solve_part2(
                &String::from("1721\n979\n366\n299\n675\n1456\n")
                    .split("\n")
                    .collect()
            ),
            241861950,
        );
    }
}
