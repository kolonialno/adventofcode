use std::str::Lines;

pub fn solve_a(numbers: &Vec<u32>) -> usize {
    let mut joltages = numbers.clone();
    joltages.sort();

    // Add charging outlet of 0 jolts
    joltages.insert(0, 0);

    // Add device's built-in adapter
    joltages.push(joltages.iter().max().unwrap() + 3);

    let differences: Vec<u32> = joltages
        .iter()
        .enumerate()
        .skip(1)
        .map(|(index, &output_joltage)| {
            let input_joltage = joltages[index - 1];
            return output_joltage - input_joltage;
        })
        .collect();

    let num_one_jolt_diffs = differences.iter().filter(|&diff| *diff == 1).count();
    let num_three_jolt_diffs = differences.iter().filter(|&diff| *diff == 3).count();
    num_one_jolt_diffs * num_three_jolt_diffs
}

pub fn solve_b(_numbers: &Vec<u32>) -> u32 {
    0
}

pub fn from_lines(lines: Lines) -> Vec<u32> {
    lines.map(|l| l.parse().unwrap()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_a() {
        let input = String::from(
            "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3",
        );
        let numbers = from_lines(input.lines());
        assert_eq!(solve_a(&numbers), 220);
    }

    #[test]
    fn example_b() {
        let input = String::from("");
        let numbers = from_lines(input.lines());
        assert_eq!(solve_b(&numbers), 0);
    }
}
