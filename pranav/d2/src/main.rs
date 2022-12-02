fn score(x: &str, y: &str) -> i32 {
    return match (x, y) {
        ("A", "X") => 1 + 3,
        ("A", "Y") => 2 + 6,
        ("A", "Z") => 3 + 0,
        ("B", "X") => 1 + 0,
        ("B", "Y") => 2 + 3,
        ("B", "Z") => 3 + 6,
        ("C", "X") => 1 + 6,
        ("C", "Y") => 2 + 0,
        ("C", "Z") => 3 + 3,
        _ => 0,
    };
}

fn predict(x: &str, y: &str) -> i32 {
    return match (x, y) {
        ("A", "X") => 3 + 0,
        ("A", "Y") => 1 + 3,
        ("A", "Z") => 2 + 6,
        ("B", "X") => 1 + 0,
        ("B", "Y") => 2 + 3,
        ("B", "Z") => 3 + 6,
        ("C", "X") => 2 + 0,
        ("C", "Y") => 3 + 3,
        ("C", "Z") => 1 + 6,
        _ => 0,
    };
}

fn a() -> i32 {
    include_str!("input.txt")
        .lines()
        .map(|l| l.split_once(" ").unwrap())
        .fold(0, |acc, (x, y)| acc + score(x, y))
}

fn b() -> i32 {
    include_str!("input.txt")
        .lines()
        .map(|l| l.split_once(" ").unwrap())
        .fold(0, |acc, (x, y)| acc + predict(x, y))
}

fn main() {
    // println!("{:?}", a());
    println!("{:?}", b());
}
