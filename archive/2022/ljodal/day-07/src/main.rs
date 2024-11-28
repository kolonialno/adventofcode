use std::collections::HashMap;

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

#[derive(Debug)]
enum FS {
    Dir(HashMap<String, FS>),
    File(usize),
}

impl FS {
    fn insert(&mut self, path: Vec<String>, name: String, entry: FS) {
        match self {
            Self::Dir(entries) => {
                if path.is_empty() {
                    entries.insert(name, entry);
                } else {
                    let fs = entries.get_mut(&path[0]).expect("foo");
                    fs.insert(path[1..path.len()].into(), name, entry);
                }
            }
            _ => panic!("Found file"),
        }
    }

    fn size(&self) -> usize {
        match self {
            Self::File(size) => size.clone(),
            Self::Dir(entries) => entries.values().map(|entry| entry.size()).sum(),
        }
    }

    fn print(&self, name: String, indent: usize) {
        match self {
            Self::Dir(entries) => {
                println!("{:indent$}- {name}: (dir)", "", indent = indent);
                for (name, entry) in entries.iter() {
                    entry.print(name.into(), indent + 4);
                }
            }
            Self::File(size) => println!("{:indent$}- {name} ({size})", "", indent = indent),
        };
    }

    /// Get the name and size of all directories
    fn dirs(&self) -> Vec<&FS> {
        let mut dirs: Vec<&FS> = vec![];

        if let Self::Dir(entries) = self {
            dirs.push(self);
            for entry in entries.values() {
                for fs in entry.dirs() {
                    dirs.push(fs);
                }
            }
        }

        dirs
    }
}

fn recreate_fs(input: &str) -> FS {
    let commands = parse_commands(input);

    let mut fs: FS = FS::Dir(HashMap::new());
    let mut current_path: Vec<String> = vec![];

    for command in commands {
        match command {
            Command::LS { output } => {
                for line in output {
                    let (a, name) = line.split_once(" ").expect("Invalid ls output");
                    if a == "dir" {
                        fs.insert(current_path.clone(), name.into(), FS::Dir(HashMap::new()));
                    } else {
                        let size = a.parse::<usize>().expect("Invalid size");
                        fs.insert(current_path.clone(), name.into(), FS::File(size));
                    }
                }
            }
            Command::CD { into } => {
                if into == ".." {
                    current_path.remove(current_path.len() - 1);
                } else if into == "/" {
                    current_path = vec![];
                } else {
                    current_path.push(into.into());
                }
            }
        }
    }

    fs
}

fn solve_part1(input: &str) -> usize {
    let fs = recreate_fs(input);
    // fs.print("/".into(), 0);

    let dirs: Vec<usize> = fs.dirs().iter().map(|dir| dir.size()).collect();
    let limit: usize = 100000;
    dirs.iter().filter(|size| **size < limit).sum()
}

fn solve_part2(input: &str) -> usize {
    let fs = recreate_fs(input);

    let disk_size: usize = 70000000;
    let needed_space: usize = 30000000;
    let currently_free = disk_size - fs.size();
    let need_to_delete = needed_space - currently_free;

    let mut dirs: Vec<&FS> = fs.dirs();
    dirs.sort_by(|a, b| a.size().cmp(&b.size()));

    let to_delete = dirs
        .iter()
        .skip_while(|dir| dir.size() < need_to_delete)
        .next()
        .expect("No directories large enough to delete");

    to_delete.size()
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
        assert_eq!(solve_part1(input), 95437, "Wrong result for pt. 1");
    }

    #[test]
    fn test_part2() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part2(input), 24933642, "Wrong result for pt. 2");
    }
}
