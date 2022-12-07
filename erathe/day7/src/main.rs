use std::{collections::HashMap, str::FromStr};

type Target = String;
type Size = i64;

fn main() {
    let input = include_str!("../input.txt");
    let mut folders: HashMap<String, Folder> = HashMap::new();
    let mut active_folder = "/".to_string();
    let mut stack = Vec::new();

    for line in input.lines().map(|l| l.parse::<Line>().unwrap()) {
        match line.0 {
            LineType::Info(InfoType::File(s)) => {
                folders
                    .entry(active_folder.clone())
                    .and_modify(|f| f.size += s);
            }
            LineType::Info(InfoType::Dir(folder)) => {
                folders
                    .entry(active_folder.clone())
                    .and_modify(|f| f.sub_folders.push(folder + &active_folder));
            }
            LineType::Command(CommandType::Cd(folder)) => match folder.as_str() {
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
            LineType::Command(CommandType::Ls) => continue,
        };
    }

    // part 1
    let mut res_1 = Vec::new();
    let (_, res) = dfs(&folders, String::from("//"), &mut res_1, 100000);
    println!("{:?}", res.iter().sum::<i64>());

    // part 2
    let mut res_2 = Vec::new();
    let (tot, res) = dfs(&folders, String::from("//"), &mut res_2, i64::MAX);
    let needed = tot - 40000000;
    println!("{:?}", res.iter().filter(|s| *s >= &needed).min().unwrap());
}

fn dfs<'a>(
    folders: &HashMap<String, Folder>,
    v: String,
    res: &'a mut Vec<i64>,
    treshold: i64,
) -> (i64, &'a mut Vec<i64>) {
    let folder = folders.get(&v).unwrap();
    let mut tot_size = folder.size;
    for sub_folder in &folder.sub_folders {
        tot_size += dfs(folders, sub_folder.clone(), res, treshold).0;
    }
    if tot_size <= treshold {
        res.push(tot_size)
    }
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
enum LineType {
    Command(CommandType),
    Info(InfoType),
}

#[derive(Debug)]
struct Line(LineType);

impl FromStr for Line {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split(' ').collect();
        if s.starts_with('$') {
            return Ok(Self(LineType::Command(match parts[1] {
                "cd" => CommandType::Cd(parts[2].into()),
                "ls" => CommandType::Ls,
                _ => panic!("wtf"),
            })));
        }

        Ok(Self(LineType::Info(match parts[0] {
            "dir" => InfoType::Dir(parts[1].into()),
            _ => InfoType::File(parts[0].parse::<i64>().unwrap()),
        })))
    }
}
