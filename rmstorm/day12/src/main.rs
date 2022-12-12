use core::fmt;

use colored::Colorize;
use pathfinding::directed::bfs::bfs;

struct Map {
    width: usize,
    heigth: usize,
    end: usize,
    start: usize,
    elements: Vec<u8>,
    solution: Option<Vec<usize>>,
}

impl From<&[u8]> for Map {
    fn from(input: &[u8]) -> Self {
        let width = input.iter().position(|e| '\n' as u8 == *e).unwrap() as usize;
        let mut heigth = 0;
        let mut start = 0;
        let mut end = 0;
        let mut elements = vec![];
        for (i, e) in input.iter().enumerate() {
            match *e as char {
                'S' => {
                    start = i - heigth;
                    elements.push('a' as u8);
                }
                'E' => {
                    end = i - heigth;
                    elements.push('z' as u8);
                }
                '\n' => {
                    heigth += 1;
                }
                e => {
                    elements.push(e as u8);
                }
            }
        }
        Map {
            width,
            heigth,
            start,
            end,
            elements,
            solution: None,
        }
    }
}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "start: {}, end: {}\n", self.start, self.end).unwrap();
        for y in 0..self.heigth + 1 {
            for x in 0..self.width {
                write!(f, "{}", self.element_as_char(x + y * self.width)).unwrap();
            }
            write!(f, "\n").unwrap();
        }
        write!(f, "\n")
    }
}

impl Map {
    fn element_as_char(&self, i: usize) -> String {
        let char = format!("{}", self.elements[i] as char);
        match &self.solution {
            Some(sol) => {
                if sol.contains(&i) {
                    char.on_blue().to_string()
                } else {
                    char
                }
            }
            None => char,
        }
    }
    fn accesible(&self, i: &usize) -> Vec<usize> {
        let cur_heigth = self.elements[*i];
        let mut accessibles = vec![];
        self.heigth;
        self.width;
        if (i % self.width) > 0 {
            accessibles.push(i - 1)
        };
        if (i % self.width) < self.width - 1 {
            accessibles.push(i + 1)
        };
        if (i / self.width) > 0 {
            accessibles.push(i - self.width)
        };
        if (i / self.width) < self.heigth {
            accessibles.push(i + self.width)
        };
        accessibles
            .into_iter()
            .filter(|i| self.elements[*i] - 2 < cur_heigth)
            .collect()
    }
}

fn main() {
    let input = include_bytes!("input.txt");
    let mut map = Map::from(&input[..]);

    map.solution = bfs(&map.start, |p| map.accesible(p), |p| p == &map.end);
    dbg!(&map);
    dbg!(map.solution.as_ref().unwrap().len() - 1);

    map.solution = map
        .elements
        .iter()
        .enumerate()
        .filter_map(|(i, e)| {
            if *e as char == 'a' {
                bfs(&i, |p| map.accesible(p), |p| p == &map.end)
            } else {
                None
            }
        })
        .reduce(|v_cur, v_new| {
            if v_new.len() < v_cur.len() {
                v_new
            } else {
                v_cur
            }
        });
    dbg!(&map);
    dbg!(map.solution.as_ref().unwrap().len() - 1);
}
