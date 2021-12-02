use std::str::FromStr;

fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(input));
    println!("{}", part2(input));
}

fn part2(input: &str) -> i32 {
    let mut sub = Sub::default();
    for c in input.lines().map(|l| l.parse::<Command>().unwrap()) {
        match c.dir {
            b'f' => sub.adjust_pos(c.len),
            b'd' => sub.adjust_aim(c.len),
            b'u' => sub.adjust_aim(-c.len),
            _ => unreachable!(),
        }
    }
    sub.get_position()
}

fn part1(input: &str) -> i32 {
    let (x, y) = input.lines().map(|l| l.parse::<Command>().unwrap()).fold(
        (0, 0),
        |(x, y), Command { dir, len }| match dir {
            b'f' => (x + len, y),
            b'd' => (x, y + len),
            b'u' => (x, y - len),
            _ => unreachable!(),
        },
    );
    x * y
}

struct Command {
    dir: u8,
    len: i32,
}

impl FromStr for Command {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split: Vec<&str> = s.split_whitespace().collect();
        Ok(Self {
            dir: split[0].as_bytes()[0],
            len: split[1].parse().unwrap(),
        })
    }
}

#[derive(Default, Debug)]
struct Sub {
    aim: i32,
    pos: (i32, i32),
}

impl Sub {
    fn get_position(&self) -> i32 {
        self.pos.0 * self.pos.1
    }

    fn adjust_aim(&mut self, adj: i32) {
        self.aim += adj
    }

    fn adjust_pos(&mut self, len: i32) {
        self.pos = (self.pos.0 + len, self.pos.1 + self.aim * len)
    }
}
