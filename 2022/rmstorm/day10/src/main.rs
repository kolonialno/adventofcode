use regex::Regex;

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Noop,
    Addx(i32),
}

fn main() {
    let input = include_str!("input.txt");
    let re = Regex::new(r"^(\w+)( (.+))?$").unwrap();
    let commands: Vec<Instruction> = input
        .lines()
        .map(|line| {
            let cap = re.captures(line).unwrap();
            match &cap[1] {
                "noop" => return Instruction::Noop,
                "addx" => return Instruction::Addx(i32::from_str_radix(&cap[3], 10).unwrap()),
                _ => panic!("Oh noO"),
            }
        })
        .collect();
    let mut x_register: Vec<i32> = vec![1];
    for command in commands {
        let current_value = *x_register.last().unwrap();
        match command {
            Instruction::Noop => x_register.push(current_value),
            Instruction::Addx(diff) => {
                x_register.push(current_value);
                x_register.push(current_value + diff);
            }
        }
    }

    dbg!((20..=220)
        .step_by(40)
        .map(|index| index as i32 * x_register[index - 1])
        .sum::<i32>());

    for (i, x) in x_register.into_iter().enumerate() {
        if (x - 1..=x + 1).contains(&(i as i32 % 40)) {
            print!("#")
        } else {
            print!(".")
        }
        if (i + 1) % 40 == 0 {
            print!("\n");
        };
    }
}
