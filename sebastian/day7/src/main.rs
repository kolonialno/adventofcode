use crate::Cmd::Cd;
use std::str::Lines;

fn main() {
    // Part 1
    let mut lines = include_str!("../input.txt").lines();
    let fs = derive_fs(&mut lines);
    println!("Small dir size sums: {}", small_dir_sizes(&fs));

    // Part 2
    let total_space = 70000000;
    let needed_space = match fs[0] {
        Fs::Dir { size, .. } => {
            println!("Currently using {size}/{total_space}");
            let free_space = total_space - size;
            30000000 - free_space
        }
        Fs::File { .. } => panic!("Expected top level item to be a dir"),
    };
    println!("We need {needed_space} more space.");
    let candidates = dirs_larger_than(&fs, needed_space);
    let smallest_candidate = candidates.iter().min();
    println!(
        "We found {} candidate dirs. Smallest: {}",
        candidates.len(),
        smallest_candidate.unwrap()
    );
}

#[derive(Debug, PartialEq)]
enum CdType<'a> {
    Up,
    Down(&'a str),
}

#[derive(Debug, PartialEq)]
enum Cmd<'a> {
    Cd(CdType<'a>),
    Ls,
}

#[derive(Debug, PartialEq)]
enum Input<'a> {
    Cmd(Cmd<'a>),
    Dir(&'a str),
    File { size: usize, name: &'a str },
}

#[derive(Debug, PartialEq)]
enum Fs<'a> {
    Dir {
        name: &'a str,
        size: usize,
        children: Vec<Fs<'a>>,
    },
    File {
        name: &'a str,
        size: usize,
    },
}

mod parser {
    use crate::Cmd::{Cd, Ls};
    use crate::{CdType, Cmd, Input};
    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while1};
    use nom::character::is_digit;
    use nom::IResult;

    fn cd(i: &str) -> IResult<&str, Cmd> {
        let (path, _) = tag("cd ")(i)?;
        match path {
            ".." => Ok((i, Cd(CdType::Up))),
            dir => Ok((i, Cd(CdType::Down(dir)))),
        }
    }

    fn ls(i: &str) -> IResult<&str, Cmd> {
        let (i, _) = tag("ls")(i)?;
        Ok((i, Ls))
    }

    fn cmd(i: &str) -> IResult<&str, Input> {
        let (i, _) = tag("$ ")(i)?;
        let (i, cmd) = alt((cd, ls))(i)?;
        Ok((i, Input::Cmd(cmd)))
    }

    pub fn is_char_digit(chr: char) -> bool {
        chr.is_ascii() && is_digit(chr as u8)
    }

    fn file(i: &str) -> IResult<&str, Input> {
        let (i, size) = take_while1(is_char_digit)(i)?;
        let (name, _) = tag(" ")(i)?;
        Ok((
            i,
            Input::File {
                size: size.parse::<usize>().unwrap(),
                name,
            },
        ))
    }

    fn dir(i: &str) -> IResult<&str, Input> {
        let (name, _) = tag("dir ")(i)?;
        Ok((i, Input::Dir(name)))
    }

    pub(crate) fn input(i: &str) -> IResult<&str, Input> {
        alt((cmd, file, dir))(i)
    }
}

fn size_of_children(children: &Vec<Fs>) -> usize {
    let mut size = 0;
    for child in children {
        match child {
            Fs::Dir { size: dir_size, .. } => size += dir_size,
            Fs::File {
                size: file_size, ..
            } => size += file_size,
        }
    }
    size
}

fn derive_fs<'a>(lines: &mut Lines<'a>) -> Vec<Fs<'a>> {
    let mut children = vec![];
    while let Some(i) = lines.next() {
        let (_, value) = parser::input(i).expect("Successful parsing...");
        match value {
            Input::Cmd(Cmd::Ls) => (), // Nothing to do here... files will follow on subsequent lines
            Input::Cmd(Cd(CdType::Up)) => {
                children.reverse();
                return children;
            }
            Input::Cmd(Cd(CdType::Down(name))) => {
                let sub_children = derive_fs(lines);
                let new_dir = Fs::Dir {
                    name,
                    size: size_of_children(&sub_children),
                    children: sub_children,
                };
                children.push(new_dir);
            }
            Input::Dir(_) => (), // nothing to do when having parsed a dir
            Input::File { size, name } => children.push(Fs::File { name, size }),
        }
    }
    children
}

fn small_dir_sizes(fss: &Vec<Fs>) -> usize {
    let mut sum = 0;
    for fs in fss {
        match fs {
            Fs::Dir { size, children, .. } => {
                if *size <= 100000 {
                    sum += size;
                }
                sum += small_dir_sizes(children);
            }
            Fs::File { .. } => {}
        }
    }
    sum
}

fn dirs_larger_than(fss: &Vec<Fs>, required_size: usize) -> Vec<usize> {
    let mut dirs = vec![];
    for fs in fss {
        match fs {
            Fs::Dir { size, children, .. } => {
                if *size >= required_size {
                    dirs.push(*size)
                }
                dirs.append(&mut dirs_larger_than(children, required_size));
            }
            Fs::File { .. } => {}
        }
    }
    dirs
}

#[cfg(test)]
mod test {
    use crate::{derive_fs, small_dir_sizes, Fs};

    #[test]
    fn test_derive_fs() {
        let mut input = include_str!("../sample.txt").lines();
        let expected = Fs::Dir {
            name: "/",
            size: 48381165,
            children: vec![
                Fs::File {
                    name: "b.txt",
                    size: 14848514,
                },
                Fs::File {
                    name: "c.dat",
                    size: 8504156,
                },
                Fs::Dir {
                    name: "a",
                    size: 94853,
                    children: vec![
                        Fs::Dir {
                            name: "e",
                            size: 584,
                            children: vec![Fs::File {
                                name: "i",
                                size: 584,
                            }],
                        },
                        Fs::File {
                            name: "h.lst",
                            size: 62596,
                        },
                        Fs::File {
                            name: "g",
                            size: 2557,
                        },
                        Fs::File {
                            name: "f",
                            size: 29116,
                        },
                    ],
                },
                Fs::Dir {
                    name: "d",
                    size: 24933642,
                    children: vec![
                        Fs::File {
                            name: "j",
                            size: 4060174,
                        },
                        Fs::File {
                            name: "d.log",
                            size: 8033020,
                        },
                        Fs::File {
                            name: "d.ext",
                            size: 5626152,
                        },
                        Fs::File {
                            name: "k",
                            size: 7214296,
                        },
                    ],
                },
            ],
        };

        assert_eq!(derive_fs(&mut input), vec![expected]);
    }

    #[test]
    fn test_calc_sum_of_small_dirs() {
        let mut lines = include_str!("../sample.txt").lines();
        let fs = derive_fs(&mut lines);
        assert_eq!(small_dir_sizes(fs), 95437)
    }
}
