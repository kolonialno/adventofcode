use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::rc::Rc;

// tree struct "inspired" by https://applied-math-coding.medium.com/a-tree-structure-implemented-in-rust-8344783abd75
struct FileSystemNode {
    parent: Option<Rc<RefCell<FileSystemNode>>>,
    // A file system node will either have a size or children. I couldnt be bothered to implement
    // this as an enum :)
    children: HashMap<String, Rc<RefCell<FileSystemNode>>>,
    size: Option<u32>,
}

impl FileSystemNode {
    fn dir_size(&self) -> u32 {
        if let Some(s) = self.size {
            return s;
        }
        self.children
            .values()
            .into_iter()
            .map(|v| v.borrow().dir_size())
            .sum()
    }

    fn to_size_vector(&self) -> Vec<u32> {
        let mut size_vector = vec![];
        if self.size == None {
            size_vector.push(self.dir_size());
        }
        for c in self.children.values() {
            size_vector.append(&mut c.borrow().to_size_vector());
        }
        size_vector
    }
}

fn parse_input(filename: &str) -> Rc<RefCell<FileSystemNode>> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);

    let root = Rc::new(RefCell::new(FileSystemNode {
        parent: None,
        size: None,
        children: HashMap::new(),
    }));
    let mut current = Rc::clone(&root);

    for line in reader.lines().map(|l| l.unwrap()) {
        if line.starts_with("$ cd") {
            let (_cmd, arg) = line[2..].split_once(" ").unwrap();
            let next;
            if arg == ".." {
                next = Rc::clone(&current.borrow().parent.as_ref().unwrap());
            } else if arg == "/" {
                next = Rc::clone(&root);
            } else {
                next = Rc::clone(&current.borrow().children.get(arg).unwrap());
            }
            current = Rc::clone(&next);
            continue;
        }
        if line.starts_with("$ ls") {
            continue;
        }
        let (arg0, name) = line.split_once(" ").unwrap();
        let child = Rc::new(RefCell::new(FileSystemNode {
            parent: Some(Rc::clone(&current)),
            size: match arg0.parse::<u32>() {
                Ok(v) => Some(v),
                Err(_) => None,
            },
            children: HashMap::new(),
        }));
        current
            .borrow_mut()
            .children
            .insert(name.to_string(), Rc::clone(&child));
    }
    root
}

fn main() {
    println!("Hello, day 7!");

    // Creating a full tree structure felt a bit too complicated for the problem we're actually
    // solving, but I didnt it anyway because all of the referene juggeling seemed like a fun
    // exercise :)
    let tree = parse_input("input.txt");

    println!(
        "Solution problem 1:{:?}",
        tree.borrow()
            .to_size_vector()
            .into_iter()
            .filter(|x| x <= &100000)
            .sum::<u32>()
    );

    let limit: i32 = tree.borrow().dir_size() as i32 - 40000000;
    println!(
        "Solution problem 2:{:?}",
        tree.borrow()
            .to_size_vector()
            .into_iter()
            .filter(|x| x >= &(limit as u32))
            .min()
    );
}
