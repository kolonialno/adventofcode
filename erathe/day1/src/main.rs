use std::time::Instant;

fn main() {
    let input = include_str!("../input.txt");

    let t1 = Instant::now();
    println!("part 1: {:?} in {:?}", part1(input), t1.elapsed());
    let t2 = Instant::now();
    println!("part 2: {:?} in {:?}", part2(input), t2.elapsed());
}

fn part1(input: &str) -> Option<u32> {
    let res = input.lines().map(|line| {
        let digits = &line
            .chars()
            .filter(|c| c.is_numeric())
            .collect::<Vec<char>>();
        [digits.first().unwrap(), digits.last().unwrap()]
            .into_iter()
            .collect::<String>()
            .parse::<u32>()
            .unwrap()
    });
    Some(res.sum())
}

fn part2(input: &str) -> Option<u64> {
    let res = input.lines().map(|line| {
        let new = line.replace("one", "o1e");
        let new = new.replace("two", "t2o");
        let new = new.replace("three", "t3e");
        let new = new.replace("four", "f4r");
        let new = new.replace("five", "f5e");
        let new = new.replace("six", "s6x");
        let new = new.replace("seven", "s7n");
        let new = new.replace("eight", "e8t");
        let new = new.replace("nine", "n9e");
        let digits = &new
            .chars()
            .filter(|c| c.is_numeric())
            .collect::<Vec<char>>();
        [digits.first().unwrap(), digits.last().unwrap()]
            .into_iter()
            .collect::<String>()
            .parse::<u64>()
            .unwrap()
    });
    Some(res.sum())
}
