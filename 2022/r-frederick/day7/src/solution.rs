use std::collections::VecDeque;

use indextree::{Arena, NodeId};

use Entry::{Dir, File};

#[derive(Debug, Clone)]
pub enum Entry {
    Dir(String),
    File(usize),
}

pub fn calculate_size(start: NodeId, arena: &Arena<Entry>) -> usize {
    match arena.get(start).unwrap().get() {
        Dir(_dir) => start
            .children(arena)
            .fold(0, |acc, n| acc + calculate_size(n, arena)),
        File(size) => *size,
    }
}

pub fn solve_part1(input: &Vec<&str>) -> usize {
    let arena = traverse(input);
    let limit = 100_000;

    arena.iter().fold(0, |acc, e| match e.get() {
        Dir(_) => {
            let start_id = arena.get_node_id(e).unwrap();
            match calculate_size(start_id, &arena) {
                x if x < limit => acc + x,
                _ => acc,
            }
        }
        File(_) => acc,
    })
}

pub fn solve_part2(input: &Vec<&str>) -> usize {
    let max_size = 70_000_000;
    let needed_space = 30_000_000;

    let arena = traverse(input);

    let free_space = max_size
        - calculate_size(
            arena.get_node_id(arena.iter().next().unwrap()).unwrap(),
            &arena,
        );
    let size_to_delete = needed_space - free_space;

    arena
        .iter()
        .fold(VecDeque::new(), |mut acc, e| match e.get() {
            Dir(_) => {
                let start_id = arena.get_node_id(e).unwrap();
                match calculate_size(start_id, &arena) {
                    x if x < size_to_delete => acc,
                    x if acc.len() == 0 || x < acc[0] => {
                        acc.push_front(x);
                        acc
                    }
                    _ => acc,
                }
            }
            File(_) => acc,
        })[0]
}

pub fn traverse<'a>(input: &'a Vec<&'a str>) -> Arena<Entry> {
    let arena = &mut Arena::new();
    let root = arena.new_node(Dir("/".to_string()));
    let mut current: NodeId = root;

    for line in input.iter() {
        match line.rsplit_once(" ") {
            Some(("$ cd", "/")) => current = root,
            Some(("$ cd", "..")) => current = current.ancestors(arena).skip(1).next().unwrap(),
            Some(("$ cd", dirname)) => {
                current = current
                    .children(arena)
                    .find(|c| {
                        if let Some(node) = arena.get(*c) {
                            match node.get() {
                                Dir(d) => return d == dirname,
                                File(_) => return false,
                            }
                        }
                        false
                    })
                    .unwrap()
            }
            Some(("$", "ls")) => continue,
            Some(("dir", dirname)) => {
                let child_dir = arena.new_node(Dir(dirname.to_string()));
                current.append(child_dir, arena)
            }
            Some((filesize, _filename)) => {
                let child_file = arena.new_node(File(filesize.parse::<usize>().unwrap()));
                current.append(child_file, arena)
            }
            _ => {
                continue;
            }
        }
    }

    arena.to_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_part1_returns_correct_answer() {
        let input = vec![
            "$ cd /",
            "$ ls",
            "dir a",
            "14848514 b.txt",
            "8504156 c.dat",
            "dir d",
            "$ cd a",
            "$ ls",
            "dir e",
            "29116 f",
            "2557 g",
            "62596 h.lst",
            "$ cd e",
            "$ ls",
            "584 i",
            "$ cd ..",
            "$ cd ..",
            "$ cd d",
            "$ ls",
            "4060174 j",
            "8033020 d.log",
            "5626152 d.ext",
            "7214296 k",
        ];

        let expected = 95437;

        assert_eq!(solve_part1(&input), expected);
    }

    #[test]
    fn solve_part2_returns_correct_answer() {
        let input = vec![
            "$ cd /",
            "$ ls",
            "dir a",
            "14848514 b.txt",
            "8504156 c.dat",
            "dir d",
            "$ cd a",
            "$ ls",
            "dir e",
            "29116 f",
            "2557 g",
            "62596 h.lst",
            "$ cd e",
            "$ ls",
            "584 i",
            "$ cd ..",
            "$ cd ..",
            "$ cd d",
            "$ ls",
            "4060174 j",
            "8033020 d.log",
            "5626152 d.ext",
            "7214296 k",
        ];

        let expected = 24933642;

        assert_eq!(solve_part2(&input), expected);
    }
}
