use std::{
    cell::RefCell,
    fs::File,
    io::{BufRead, BufReader},
    rc::{Rc, Weak},
};

use helpers::get_input_file;

#[derive(Eq, PartialEq)]
enum FileType {
    Directory,
    File,
}

struct Node {
    name: String,
    size: usize,
    children: Vec<Rc<RefCell<Node>>>,
    file_type: FileType,
    parent: Weak<RefCell<Node>>,
}

impl Node {
    fn parse_line(line: String, parent: Weak<RefCell<Node>>) -> Self {
        if line.starts_with("dir") {
            Node {
                name: String::from(line.get(4..).unwrap()),
                size: 0,
                children: Vec::default(),
                file_type: FileType::Directory,
                parent,
            }
        } else {
            let (size, name) = line.split_once(' ').unwrap();
            Node {
                name: String::from(name),
                size: size.parse::<usize>().unwrap(),
                children: Vec::default(),
                file_type: FileType::File,
                parent,
            }
        }
    }

    fn is_directory(&self) -> bool {
        self.file_type == FileType::Directory
    }

    fn size(&self) -> usize {
        match self.file_type {
            FileType::Directory => self.children.iter().map(|x| x.borrow().size()).sum(),
            FileType::File => self.size,
        }
    }

    fn find_directories(&self, dirs: &mut Vec<(String, usize)>) {
        if self.is_directory() {
            let size = self.size();
            dirs.push((self.name.clone(), size));

            for c in &self.children {
                c.borrow().find_directories(dirs);
            }
        }
    }

    fn find_all_dirs_sorted_by_size(&self) -> Vec<(String, usize)> {
        let mut dirs = Vec::default();
        self.find_directories(&mut dirs);
        dirs.sort_by_key(|x| x.1);
        dirs
    }
}

#[derive(PartialEq, Eq)]
enum CommandType {
    Ls,
    Cd,
}

struct Command {
    command_type: CommandType,
    parameter: String,
}

impl Command {
    fn parse_line(line: String) -> Self {
        if line.starts_with("$ ls") {
            Command {
                command_type: CommandType::Ls,
                parameter: String::default(),
            }
        } else {
            Command {
                command_type: CommandType::Cd,
                parameter: String::from(line.get(5..).unwrap()),
            }
        }
    }

    fn go_up(&self) -> bool {
        self.parameter == ".."
    }
}

fn main() {
    let file = File::open(get_input_file()).unwrap();
    let lines = BufReader::new(file).lines().flatten();
    let root = Rc::new(RefCell::new(Node {
        name: String::from("/"),
        size: 0,
        children: Vec::default(),
        file_type: FileType::Directory,
        parent: Weak::default(),
    }));
    let mut current_node = Rc::downgrade(&root);

    for line in lines.skip(1) {
        if line.starts_with('$') {
            let command = Command::parse_line(line);
            if command.command_type == CommandType::Cd {
                if command.go_up() {
                    current_node = current_node.upgrade().unwrap().borrow().parent.clone();
                } else {
                    let current_rc = current_node.upgrade().unwrap();
                    let current_refcell = current_rc.borrow();
                    let parent_rc = current_refcell
                        .children
                        .iter()
                        .find(|e| e.borrow().name == command.parameter);
                    current_node = Rc::downgrade(parent_rc.unwrap());
                }
            }
        } else {
            let node = Rc::new(RefCell::new(Node::parse_line(
                line.clone(),
                current_node.clone(),
            )));
            let strong = current_node.upgrade().unwrap();
            strong.borrow_mut().children.push(node);
        }
    }
    const LIMIT: usize = 100000;
    let all_dirs_sorted = root.borrow().find_all_dirs_sorted_by_size();
    println!(
        "Size below {}: {}",
        LIMIT,
        all_dirs_sorted
            .iter()
            .filter(|(_, size)| *size <= LIMIT)
            .map(|(_, size)| size)
            .sum::<usize>()
    );

    const TOTAL_SPACE: usize = 70_000_000;
    const MIN_REQUIRED_SPACE: usize = 30_000_000;
    let min_space_to_free_up =
        MIN_REQUIRED_SPACE.saturating_sub(TOTAL_SPACE.saturating_sub(root.borrow().size()));
    println!("Required space for update: {}", min_space_to_free_up);
    let smallest_dir = all_dirs_sorted
        .iter()
        .find(|(_, size)| *size >= min_space_to_free_up)
        .unwrap();
    println!(
        "Smallest directory to delete to have enough space: {} with size {}",
        smallest_dir.0, smallest_dir.1
    );
}
