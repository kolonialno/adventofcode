use std::str::FromStr;

fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(input));
    println!("{}", part2(input));
}

fn part1(input: &str) -> i32 {
    let mut coords = (0, 0);
    for c in input.lines().map(|l| l.parse::<Command>().unwrap()) {
        match c.dir.as_str() {
            "forward" => coords.0 += c.len,
            "down" => coords.1 += c.len,
            "up" => coords.1 -= c.len,
            _ => (),
        }
    }
    coords.0 * coords.1
}

fn part2(input: &str) -> i32 {
    let mut sub_pos = SubPos::default();
    for c in input.lines().map(|l| l.parse::<Command>().unwrap()) {
        match c.dir.as_str() {
            "forward" => sub_pos.adjust_pos(c.len),
            "down" => sub_pos.adjust_aim(c.len),
            "up" => sub_pos.adjust_aim(-c.len),
            _ => (),
        }
    }
    sub_pos.get_position()
}

struct Command {
    dir: String,
    len: i32,
}

impl FromStr for Command {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split: Vec<&str> = s.split(" ").collect();
        Ok(Self {
            dir: split[0].to_string(),
            len: split[1].parse().unwrap(),
        })
    }
}

#[derive(Default, Debug)]
struct SubPos {
    aim: i32,
    pos: (i32, i32),
}

impl SubPos {
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
