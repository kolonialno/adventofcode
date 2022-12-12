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
        let mut m = Map {
            width: input.iter().position(|e| '\n' as u8 == *e).unwrap() as usize,
            heigth: 0,
            start: 0,
            end: 0,
            elements: vec![],
            solution: None,
        };

        for (i, e) in input.iter().enumerate() {
            match *e as char {
                'S' => {
                    m.start = i - m.heigth;
                    m.elements.push('a' as u8);
                }
                'E' => {
                    m.end = i - m.heigth;
                    m.elements.push('z' as u8);
                }
                '\n' => {
                    m.heigth += 1;
                }
                e => {
                    m.elements.push(e as u8);
                }
            }
        }
        m
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
        let g = 9 * (self.elements[i] - 'a' as u8);
        let char = String::from_utf8_lossy(&[self.elements[i]]).truecolor(40, g, 0);
        match &self.solution {
            Some(sol) => {
                if sol.contains(&i) {
                    char.on_blue()
                } else {
                    char
                }
            }
            None => char,
        }
        .to_string()
    }
    fn accesible(&self, i: &usize) -> Vec<usize> {
        let cur_heigth = self.elements[*i];
        let mut accessibles = vec![];
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
            .filter(|j| cur_heigth < (self.elements[*j] + 2))
            .collect()
    }
}

fn main() {
    let input = include_bytes!("input.txt");
    let mut map = Map::from(&input[..]);

    dbg!(&map);
    map.solution = bfs(&map.end, |p| map.accesible(p), |p| p == &map.start);
    dbg!(&map);
    dbg!(map.solution.as_ref().unwrap().len() - 1);
    map.solution = bfs(
        &map.end,
        |p| map.accesible(p),
        |p| 'a' == map.elements[*p] as char,
    );
    dbg!(&map);
    dbg!(map.solution.as_ref().unwrap().len() - 1);
}
