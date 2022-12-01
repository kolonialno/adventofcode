fn a() -> i32 {
    let elfs = include_str!("input.txt").split("\n\n").map(|e| {
        e.split("\n")
            .fold(0, |acc, f| acc + f.parse::<i32>().unwrap())
    });

    elfs.max().unwrap()
}

fn b() -> i32 {
    let mut elfs = include_str!("input.txt")
        .split("\n\n")
        .map(|e| {
            e.split("\n")
                .fold(0, |acc, f| acc + f.parse::<i32>().unwrap())
        })
        .collect::<Vec<i32>>();

    elfs.sort();
    elfs.reverse();
    elfs.into_iter().take(3).sum()
}

fn main() {
    println!("{}", a());
    println!("{}", b());
}
