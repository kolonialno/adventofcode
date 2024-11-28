use crate::{Solution, SolutionPair};

///////////////////////////////////////////////////////////////////////////////

pub fn solve() -> SolutionPair {
    use std::fs::File;
    use std::io::{BufRead, BufReader};

    let reader = BufReader::new(File::open("input/input.txt").expect("Cannot open file.txt"));

    let mut max_calories_observed: [u32; 3] = [0, 0, 0];
    let mut accumulated_calories = 0;

    reader.lines().for_each(|line| {
        let parsed_string = line.expect("Can't parse line");
        if parsed_string.is_empty() {
            if accumulated_calories > max_calories_observed[0] {
                max_calories_observed[0] = accumulated_calories;
                max_calories_observed.sort_unstable();
            }
            accumulated_calories = 0;
        } else {
            accumulated_calories += parsed_string.parse::<u32>().unwrap()
        }
    });

    (
        Solution::U32(max_calories_observed.last().unwrap().clone()),
        Solution::U32(max_calories_observed.iter().sum::<u32>()),
    )
}
