use std::{collections::HashMap, str::FromStr, time::Instant};

type Target = String;
type Size = i64;

fn main() {
    let input = include_str!("../input.txt");

    let t = Instant::now();
    let tree = initialize_arena_tree(input);
    // part 1 arena
    let r = tree
        .arena
        .iter()
        .filter(|node| node.val <= 100000)
        .map(|node| node.val)
        .sum::<i64>();
    println!("part 1: {r}");

    // part 2 arena
    let r = tree
        .arena
        .iter()
        .filter(|node| node.val >= tree.arena[0].val - 40000000)
        .map(|node| node.val)
        .min()
        .unwrap();
    println!("part 2: {r}");
    println!("Second try finished in: {:?}", t.elapsed()); // ~270 microsecs

    //-------------------------------------------------------------------------
    let t = Instant::now();
    let folders = initialize_hash_map(input);
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
    println!("First solution finished in: {:?}", t.elapsed()); // ~480 microsecs
}

fn initialize_arena_tree(input: &str) -> ArenaTree<i64> {
    let mut tree: ArenaTree<i64> = ArenaTree::default();
    tree.arena.push(Node::new(0, None));

    for line in input.lines().skip(1).map(|l| l.parse::<Line>().unwrap()) {
        match line {
            Line::Info(InfoType::File(s)) => {
                tree.arena[tree.active_idx].val += s;
                // propagate file size up
                for idx in tree.ancestors() {
                    tree.arena[idx].val += s;
                }
            }
            Line::Command(CommandType::Cd(folder)) => match folder.as_str() {
                ".." => tree.active_idx = tree.arena[tree.active_idx].parent.unwrap(),
                _ => tree.active_idx = tree.add_node(0),
            },
            Line::Info(InfoType::Dir(_folder)) => continue,
            Line::Command(CommandType::Ls) => continue,
        };
    }
    tree
}

// inspired by https://dev.to/deciduously/no-more-tears-no-more-knots-arena-allocated-trees-in-rust-44k6
#[derive(Debug, Default)]
struct ArenaTree<T>
where
    T: PartialEq,
{
    arena: Vec<Node<T>>,
    active_idx: usize,
}

impl<T> ArenaTree<T>
where
    T: PartialEq,
{
    fn add_node(&mut self, val: T) -> usize {
        let idx = self.arena.len();
        self.arena.push(Node::new(val, Some(self.active_idx)));
        idx
    }

    fn ancestors(&self) -> Vec<usize> {
        let mut curr = self.active_idx;
        let mut ancestors = vec![];
        while let Some(p) = self.arena[curr].parent {
            ancestors.push(p);
            curr = p;
        }
        ancestors
    }
}

#[derive(Debug)]
struct Node<T>
where
    T: PartialEq,
{
    val: T,
    parent: Option<usize>,
}

impl<T> Node<T>
where
    T: PartialEq,
{
    fn new(val: T, parent: Option<usize>) -> Self {
        Self { val, parent }
    }
}

// First solution
fn initialize_hash_map(input: &str) -> HashMap<String, Folder> {
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
    folders
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
