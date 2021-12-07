use crate::util::file_by_lines;

pub fn run() {
    let positions = &file_by_lines("day07.txt")[0];
    let positions = positions
        .split(",")
        .map(|x| x.parse::<isize>().unwrap())
        .collect::<Vec<isize>>();
    let min_fuel = find_min_fuel(&positions, |d| d);
    println!("Part 1: {}", min_fuel);
    let min_fuel = find_min_fuel(&positions, |d| d * (d + 1) / 2);
    println!("Part 2: {}", min_fuel);
}

fn find_min_fuel(positions: &Vec<isize>, diff_function: fn(diff: isize) -> isize) -> isize {
    let min_position = positions.iter().min().unwrap();
    let max_position = positions.iter().max().unwrap();
    let mut min_fuel: isize = isize::max_value();

    for i in *min_position..=*max_position {
        let diffs = positions
            .iter()
            .map(|j| diff_function((i - j).abs()))
            .sum::<isize>();
        if diffs < min_fuel {
            min_fuel = diffs;
        }
    }

    min_fuel
}
