use std::fs::read_to_string;

fn find_pair(sum: usize, numbers: &[usize]) -> Option<(&usize, usize)> {
    let (_, number) = numbers
        .iter()
        .enumerate()
        .find(|(index, number)| numbers[index + 1..].binary_search(&(sum - *number)).is_ok())?;
    return Some((number, (sum - number)));
}

fn main() {
    let problem = read_to_string("../input/day01a.txt").unwrap();
    let mut numbers: Vec<usize> = problem
        .trim()
        .split("\n")
        .map(|i| i.parse::<usize>().unwrap())
        .collect();

    numbers.sort();
    let (first, second) = find_pair(2020, &numbers).unwrap();
    println!("Part 1: {}", first * second);

    let (first, second) = numbers
        .iter()
        .enumerate()
        .find_map(|(index, number)| find_pair(2020 - *number, &numbers[index + 1..]))
        .unwrap();
    println!("Part 2: {:?}", first * second * (2020 - first - second));
}
