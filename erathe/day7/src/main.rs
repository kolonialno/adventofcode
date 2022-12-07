use std::{collections::HashMap, str::FromStr};

type Target = String;
type Size = i64;

fn main() {
    let input = include_str!("../input.txt");
    let mut folders: HashMap<String, Folder> = HashMap::new();
    let mut active_folder = "/".to_string();
    let mut stack = Vec::new();

    for line in input.lines().map(|l| l.parse::<Line>().unwrap()) {
        match line {
            Line::Info(InfoType::File(s)) => {
                let entry = folders.get_mut(&active_folder).unwrap();
                entry.size += s;
            }
            Line::Info(InfoType::Dir(folder)) => {
                let entry = folders.get_mut(&active_folder).unwrap();
                entry.sub_folders.push(folder + &active_folder);
            }
            Line::Command(CommandType::Cd(folder)) => match folder.as_str() {
                ".." => {
                    active_folder = stack.pop().unwrap();
                }
                _ => {
                    stack.push(active_folder.clone());
                    active_folder = folder + &active_folder;

                    folders.insert(
                        active_folder.clone(),
                        Folder {
                            size: 0,
                            sub_folders: Vec::new(),
                        },
                    );
                }
            },
            Line::Command(CommandType::Ls) => continue,
        };
    }

    // part 1
    let mut res = Vec::new();
    let (tot, res) = dfs(&folders, String::from("//"), &mut res);
    println!(
        "part 1: {:?}",
        res.iter().filter(|s| **s <= 100000).sum::<i64>()
    );
    println!(
        "part 2: {:?}",
        res.iter().filter(|s| **s >= tot - 40000000).min().unwrap()
    );
}

fn dfs<'a>(
    folders: &HashMap<String, Folder>,
    v: String,
    res: &'a mut Vec<i64>,
) -> (i64, &'a mut Vec<i64>) {
    let folder = folders.get(&v).unwrap();
    let mut tot_size = folder.size;
    for sub_folder in &folder.sub_folders {
        tot_size += dfs(folders, sub_folder.clone(), res).0;
    }
    res.push(tot_size);
    (tot_size, res)
}

#[derive(Debug)]
struct Folder {
    size: Size,
    sub_folders: Vec<String>,
}

#[derive(Debug)]
enum CommandType {
    Cd(Target),
    Ls,
}

#[derive(Debug)]
enum InfoType {
    Dir(Target),
    File(Size),
}

#[derive(Debug)]
enum Line {
    Command(CommandType),
    Info(InfoType),
}

impl FromStr for Line {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split(' ').collect();
        if s.starts_with('$') {
            return Ok(Line::Command(match parts[1] {
                "cd" => CommandType::Cd(parts[2].into()),
                "ls" => CommandType::Ls,
                _ => panic!("wtf"),
            }));
        }

        Ok(Line::Info(match parts[0] {
            "dir" => InfoType::Dir(parts[1].into()),
            _ => InfoType::File(parts[0].parse::<i64>().unwrap()),
        }))
    }
}
