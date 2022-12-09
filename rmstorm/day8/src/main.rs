use std::{char::ParseCharError, fmt, str::FromStr};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Vec2 {
    x: usize,
    y: usize,
}

struct Map {
    size: Vec2,
    elements: Vec<u32>,
}

impl FromStr for Map {
    type Err = ParseCharError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let x = s.split_once('\n').unwrap().0.chars().count();
        let mut y = 0;
        let elements: Vec<u32> = s
            .lines()
            .flat_map(|e| {
                y += 1;
                e.chars()
            })
            .map(|c| c.to_digit(10).unwrap())
            .collect();
        Ok(Map {
            size: Vec2 { x, y },
            elements,
        })
    }
}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", '\n').unwrap();
        for y in 0..self.size.y {
            for x in 0..self.size.x {
                write!(f, "{}", self.elements[x + y * self.size.x]).unwrap();
            }
            write!(f, "{}", '\n').unwrap();
        }
        write!(f, "{}", '\n')
    }
}

impl Map {
    fn index_to_pos(&self, index: usize) -> Vec2 {
        if index >= self.size.x * self.size.y {
            panic!("Index is too big for map! {:?},{:?}", self.size, index);
        }
        Vec2 {
            x: (index % self.size.x),
            y: (index / self.size.y),
        }
    }
    fn get(&self, x: usize, y: usize) -> u32 {
        self.elements[x + self.size.x * y]
    }
    fn check_visible(&self, pos: Vec2) -> bool {
        let heigth = self.get(pos.x, pos.y);
        match (
            (0..pos.x).find(|x| self.get(*x, pos.y) >= heigth),
            ((pos.x + 1)..self.size.x).find(|x| self.get(*x, pos.y) >= heigth),
            (0..pos.y).find(|y| self.get(pos.x, *y) >= heigth),
            ((pos.y + 1)..self.size.y).find(|y| self.get(pos.x, *y) >= heigth),
        ) {
            (Some(_), Some(_), Some(_), Some(_)) => return false,
            _ => return true,
        }
    }
    fn scenic_score(&self, pos: Vec2) -> usize {
        let heigth = self.get(pos.x, pos.y);
        match (
            (0..pos.x).rev().find(|x| self.get(*x, pos.y) >= heigth),
            ((pos.x + 1)..self.size.x).find(|x| self.get(*x, pos.y) >= heigth),
            (0..pos.y).rev().find(|y| self.get(pos.x, *y) >= heigth),
            ((pos.y + 1)..self.size.y).find(|y| self.get(pos.x, *y) >= heigth),
        ) {
            (Some(a), Some(b), Some(c), Some(d)) => {
                (pos.x - a) * (b - pos.x) * (pos.y - c) * (d - pos.y)
            }
            _ => 0,
        }
    }
}

fn main() {
    let input = include_str!("input.txt");
    let mut map = Map::from_str(input).unwrap();
    let mut visible = 0;
    for index in 0..map.elements.len() {
        let pos = map.index_to_pos(index);
        if pos.x == 0 || pos.x == map.size.x - 1 || pos.y == 0 || pos.y == map.size.y - 1 {
            visible += 1;
        } else {
            if map.check_visible(pos) {
                visible += 1;
            }
        }
    }

    dbg!(&map);
    dbg!(visible);
    for index in 0..map.elements.len() {
        let pos = map.index_to_pos(index);
        if pos.x == 0 || pos.x == map.size.x - 1 || pos.y == 0 || pos.y == map.size.y - 1 {
            map.elements[index] = 10;
        }
    }

    dbg!((0..map.elements.len())
        .map(|e| {
            let pos = map.index_to_pos(e);
            map.scenic_score(pos)
        })
        .max()
        .unwrap());
}

#[cfg(test)]
mod tests {
    use crate::Vec2;

    #[test]
    fn test_index_to_pos() {
        let map = crate::Map {
            size: Vec2 { x: 7, y: 7 },
            elements: vec![0; 48],
        };
        assert_eq!(map.index_to_pos(0), Vec2 { x: 0, y: 0 });
        assert_eq!(map.index_to_pos(1), Vec2 { x: 1, y: 0 });
        assert_eq!(map.index_to_pos(6), Vec2 { x: 6, y: 0 });
        assert_eq!(map.index_to_pos(7), Vec2 { x: 0, y: 1 });
        assert_eq!(map.index_to_pos(48), Vec2 { x: 6, y: 6 });

        let result =
            std::panic::catch_unwind(|| assert_eq!(map.index_to_pos(49), Vec2 { x: 7, y: 7 }));
        assert!(result.is_err()); //probe further for specific error type here, if desired
    }
}
