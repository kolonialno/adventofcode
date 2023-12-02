use std::{collections::HashMap, str::FromStr};

use anyhow::Context;

fn main() {
    let input = include_str!("../input.txt");

    println!("Part one: {}", part_one(input));
    println!("Part two: {}", part_two(input));
}

#[derive(Debug, Eq, PartialEq)]
struct CubeSet {
    red: u32,
    green: u32,
    blue: u32,
}

impl CubeSet {
    fn can_contain(&self, other: &CubeSet) -> bool {
        self.red >= other.red && self.green >= other.green && self.blue >= other.blue
    }

    fn power(&self) -> u32 {
        self.red * self.green * self.blue
    }
}

impl FromStr for CubeSet {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let counts: HashMap<&str, u32> = s
            .split(",")
            .filter_map(|color_amount| {
                let (count, color) = color_amount.trim().split_once(" ")?;
                Some((color, count.parse::<u32>().ok()?))
            })
            .collect();

        Ok(Self {
            red: *counts.get("red").unwrap_or(&0),
            green: *counts.get("green").unwrap_or(&0),
            blue: *counts.get("blue").unwrap_or(&0),
        })
    }
}

#[derive(Debug)]
struct Game {
    num: u32,
    observed_cube_sets: Vec<CubeSet>,
}

impl Game {
    fn is_valid_for(&self, cube_set: &CubeSet) -> bool {
        self.observed_cube_sets
            .iter()
            .all(|observed_cube_set| cube_set.can_contain(observed_cube_set))
    }

    fn minimal_cube_set(&self) -> CubeSet {
        let mut minimal_cube_set = CubeSet {
            red: 0,
            green: 0,
            blue: 0,
        };

        for observed_cube_set in &self.observed_cube_sets {
            if observed_cube_set.red > minimal_cube_set.red {
                minimal_cube_set.red = observed_cube_set.red;
            }
            if observed_cube_set.green > minimal_cube_set.green {
                minimal_cube_set.green = observed_cube_set.green;
            }
            if observed_cube_set.blue > minimal_cube_set.blue {
                minimal_cube_set.blue = observed_cube_set.blue;
            }
        }

        minimal_cube_set
    }
}

impl FromStr for Game {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (game_prefix, observed_cube_sets) = s.split_once(":").context("No colon in string")?;

        let num = game_prefix
            .split_once(" ")
            .ok_or(anyhow::anyhow!("No space in game spec"))?
            .1
            .parse::<u32>()
            .context("No game number")?;

        let observed_cube_sets = observed_cube_sets
            .split(";")
            .filter_map(|s| CubeSet::from_str(s).ok())
            .collect();

        Ok(Self {
            num,
            observed_cube_sets,
        })
    }
}

fn part_one(input: &str) -> u32 {
    let comparison_cube_set = CubeSet {
        red: 12,
        green: 13,
        blue: 14,
    };
    input
        .lines()
        .filter_map(|line| {
            let game = line.parse::<Game>().ok()?;
            game.is_valid_for(&comparison_cube_set).then_some(game.num)
        })
        .sum::<u32>()
}

fn part_two(input: &str) -> u32 {
    input
        .lines()
        .filter_map(|line| {
            let game = line.parse::<Game>().ok()?;
            Some(game.minimal_cube_set().power())
        })
        .sum::<u32>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sample_part_one() {
        let sample = include_str!("../sample.txt");
        assert_eq!(part_one(sample), 8);
    }

    #[test]
    fn sample_part_two() {
        let sample = include_str!("../sample.txt");
        assert_eq!(part_two(sample), 2286);
    }
}
