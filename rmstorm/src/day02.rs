use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};
use std::str::FromStr;

#[derive(Debug, PartialEq)]
enum Command {
  Up(u32),
  Down(u32),
  Forward(u32),
}

impl FromStr for Command {
  type Err = Error;

  fn from_str(command: &str) -> Result<Self, Self::Err> {
      let command: Vec<&str> = command.split(" ").collect();
      Ok(match command[0] {
        "up" => Command::Up(command[1].parse().unwrap()),
        "down" => Command::Down(command[1].parse().unwrap()),
        "forward" => Command::Forward(command[1].parse().unwrap()),
        _ => panic!("oh no!"),
      })
  }
}

fn read<R: Read>(io: R) -> Result<Vec<Command>, Error> {
    let br = BufReader::new(io);
    let mut v = vec![];
    
    for line in br.lines() {
        v.push(line?.trim().parse()?);
    }
    Ok(v)
}

pub fn day02() {
    let v = read(File::open("inputs/day02.txt").unwrap()).unwrap();
    let mut horizontal = 0;
    let mut depth = 0;
    for c in v.iter() {
      match c {
        Command::Up(i) => depth -= i,
        Command::Down(i) => depth += i,
        Command::Forward(i) => horizontal += i,
      }
    }
    println!("answer 1 = {:#?}", horizontal*depth);

    horizontal = 0;
    depth = 0;
    let mut aim = 0;
    for c in v.iter() {
      match c {
        Command::Up(i) => aim -= i,
        Command::Down(i) => aim += i,
        Command::Forward(i) => {
          horizontal += i;
          depth += aim*i
        },
      }
    }
    println!("answer 2 = {:#?}", horizontal*depth);
}
