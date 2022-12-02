fn main() {
    let input = include_str!("input.txt");

    let mut elf_calories = input
        .split("\n\n")
        .map(|e| {
            e.split("\n")
                .map(|e| e.parse::<i32>().unwrap())
                .sum::<i32>()
        })
        .collect::<Vec<i32>>();

    dbg!(elf_calories.iter().max().unwrap());

    elf_calories.sort();
    elf_calories.reverse();
    dbg!(&elf_calories[..3].iter().sum::<i32>());
}
