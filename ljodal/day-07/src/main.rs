#[derive(Debug)]
enum Command<'a> {
    LS { output: Vec<&'a str> },
    CD { into: &'a str },
}

fn parse_command(input: &str) -> Command {
    let lines: Vec<&str> = input.lines().collect();
    let command: Vec<&str> = lines[0].split_whitespace().collect();

    match command[0] {
        "cd" => Command::CD { into: command[1] },
        "ls" => Command::LS {
            output: lines[1..lines.len()].into(),
        },
        _ => unreachable!("Unexpected command"),
    }
}

fn parse_commands(input: &str) -> Vec<Command> {
    input
        .split("$ ")
        .filter(|line| !line.is_empty())
        .map(parse_command)
        .collect()
}

struct Dir<'a> {
    parent: Option<Box<Dir<'a>>>,
    name: &'a str,
    dirs: Vec<Dir<'a>>,
    files: Vec<(&'a str, usize)>,
}

fn recreate_fs(commands: Vec<Command>) -> Dir {
    let mut root = Dir {
        parent: None,
        name: "",
        dirs: vec![],
        files: vec![],
    };

    let mut current = &root;
    for command in commands {
        match command {
            Command::LS { output } => {}
            Command::CD { into } => {
                if into == ".." {
                    current = &current.parent.expect("Cannot CD beyond root");
                } else if into == "/" {
                    current = &root;
                } else {
                    current = current
                        .dirs
                        .iter()
                        .find(|dir| dir.name == into)
                        .expect("cd into unknown dir");
                }
            }
        }
    }

    root
}

fn solve_part1(input: &str) -> usize {
    let commands = parse_commands(input);
    println!("Commands: {commands:?}");
    0
}

fn solve_part2(input: &str) -> usize {
    0
}

fn main() {
    let input = include_str!("./input.txt");
    let sum = solve_part1(input);
    println!("Part 1: {sum}");
    let sum = solve_part2(input);
    println!("Part 2: {sum}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part1(input), 95437);
    }

    #[test]
    fn test_part2() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part2(input), 4);
    }
}
