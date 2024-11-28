use std::fs::read_to_string;

#[derive(Debug)]
struct Map {
    content: Vec<String>,
    width: usize,
    height: usize,
}

impl From<String> for Map {
    fn from(content: String) -> Self {
        let content: Vec<String> = content.trim().split("\n").map(|i| i.to_owned()).collect();
        let width = content[0].len();
        let height = content.len();
        Map {
            content,
            width,
            height,
        }
    }
}

impl Map {
    fn tree_at(self: &Self, row: usize, column: usize) -> bool {
        let column = column % self.width;
        return self.content[row][column..=column] == *"#";
    }

    fn traverse_by(self: &Self, row_step: &usize, column_step: &usize) -> Vec<bool> {
        return (0..self.height)
            .step_by(*row_step)
            .enumerate()
            .map(|(step, row)| self.tree_at(row, (step * column_step) % self.width))
            .collect();
    }
}

fn main() {
    let problem = read_to_string("../input/day03.txt").unwrap();
    let map: Map = problem.into();
    let encountered_trees: usize = map.traverse_by(&1, &3).iter().map(|i| *i as usize).sum();
    println!("Task 1: {}", encountered_trees);

    let product: usize = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        .iter()
        .map(|(right, down)| {
            map.traverse_by(down, right)
                .iter()
                .map(|i| *i as usize)
                .sum::<usize>()
        })
        .product();
    println!("Task 2: {}", product);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tree_at() {
        let content = String::from(".#\n#.");
        let map: Map = content.into();
        assert!(map.tree_at(0, 1));
        assert!(map.tree_at(0, 3));
        assert!(!map.tree_at(0, 0));
    }

    #[test]
    fn test_traverse_by() {
        let content = String::from("...\n..#\n.#.");
        let map: Map = content.into();
        assert_eq!(map.traverse_by(&1, &2), vec![false, true, true],);
    }
}
