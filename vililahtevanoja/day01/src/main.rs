
fn parse(data: &str) -> Vec<Vec<u32>> {
    data
    .split("\n\n")
    .map(|s| s.split("\n")
        .filter_map(|n| n.parse::<u32>().ok())
        .collect()
    ).collect()
}

fn sums(groups: Vec<Vec<u32>>) -> Vec<u32> {
    groups.iter().map(|v| v.iter().sum()).collect() 
}

fn solve1(data: &str) -> u32 {
    let groups = parse(data);
    
    let group_sums = sums(groups);

    *group_sums.iter().max().expect("should not be empty")
}

fn solve2(data: &str) -> u32 {
    let groups = parse(data);
    let mut sums = sums(groups);
    sums.sort();
    sums.reverse();
    sums.iter().take(3).sum()
}

fn main() {
    let data = include_str!("../input.txt");
    println!("Part 1: {}", solve1(data));
    println!("Part 2: {}", solve2(data));
}

#[test]
fn test_part1() {
    let data = include_str!("../example.txt");
    assert_eq!(solve1(data), 24000)
}

#[test]
fn test_part2() {
    let data = include_str!("../example.txt");
    assert_eq!(solve2(data), 45000)
}