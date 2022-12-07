#[derive(Debug)]
enum Item {
    Dir { name: String, items: Vec<Item> },
    File { name: String, size: i64 },
}

fn get_subdir_by_path<'a, 'b>(items: &'a mut Vec<Item>, dir_name: &'b str) -> &'a mut Vec<Item> {
    for item in items {
        if let Item::Dir { name, items } = item {
            if name == dir_name {
                return items;
            }
        }
    }
    panic!("This shouldn't happen {}", dir_name);
}

impl Item {
    fn get_dir_content(&mut self) -> &mut Vec<Item> {
        match self {
            Item::Dir { name: _, items } => items,
            _ => panic!("pls dont"),
        }
    }
    fn get_dir_content_at_path(&mut self, path: &Vec<&str>) -> &mut Vec<Item> {
        let mut items = self.get_dir_content();
        for p in &path[1..] {
            items = get_subdir_by_path(items, p)
        }
        items
    }
    fn calculate_dir_size(&mut self, total_size: &mut i64, all_sizes: &mut Vec<i64>) -> i64 {
        let mut dir_size = 0;
        for item in self.get_dir_content() {
            if let Item::File { name: _, size } = item {
                dir_size += *size;
            } else {
                dir_size += item.calculate_dir_size(total_size, all_sizes);
            }
        }
        if dir_size <= 100000 {
            *total_size += dir_size
        }
        all_sizes.push(dir_size);
        dir_size
    }
}

fn main() {
    let input = include_str!("input.txt");
    let mut root_dir = Item::Dir {
        name: "/".to_string(),
        items: vec![],
    };
    let mut path: Vec<&str> = vec!["/"];
    let mut cur_dir = root_dir.get_dir_content_at_path(&path);
    for line in input.lines() {
        if line.starts_with('$') {
            if line.starts_with("$ cd ") {
                let new_dir: &str = line.split("$ cd ").last().unwrap();
                if ".." == new_dir {
                    path.pop().unwrap();
                } else if "/" != new_dir {
                    path.push(new_dir);
                }
                cur_dir = root_dir.get_dir_content_at_path(&path);
            }
        } else {
            if line.starts_with("dir") {
                let dir_parts: Vec<&str> = line.split(' ').collect();
                cur_dir.push(Item::Dir {
                    name: dir_parts[1].to_string(),
                    items: vec![],
                });
            } else {
                let file_parts: Vec<&str> = line.split(' ').collect();
                cur_dir.push(Item::File {
                    name: file_parts[1].to_string(),
                    size: i64::from_str_radix(file_parts[0], 10).unwrap(),
                })
            }
        }
    }

    let mut total_weird_size: i64 = 0;
    let mut all_sizes: Vec<i64> = vec![];
    let full_size = root_dir.calculate_dir_size(&mut total_weird_size, &mut all_sizes);
    dbg!(&total_weird_size);
    all_sizes.sort();
    for s in all_sizes {
        if full_size-s < 40000000 {
            dbg!(s);
            return
        }
    }
}
