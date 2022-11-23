use std::collections::HashMap;
use std::collections::HashSet;

use std::fmt;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq)]
struct DisplayPuzzle {
    inputs: Vec<HashSet<u8>>,
    outputs: Vec<HashSet<u8>>,
}

pub fn get_final_mapping(
    puzzle_solution: &HashMap<&str, HashSet<u8>>,
) -> HashMap<i32, HashSet<u8>> {
    let true_digits = HashMap::from([
        ("abcefg", 0),
        ("cf", 1),
        ("acdeg", 2),
        ("acdfg", 3),
        ("bcdf", 4),
        ("abdfg", 5),
        ("abdefg", 6),
        ("acf", 7),
        ("abcdefg", 8),
        ("abcdfg", 9),
    ]);

    let mut final_mapping: HashMap<i32, HashSet<u8>> = HashMap::new();
    for (dig, d) in true_digits {
        final_mapping.insert(
            d,
            HashSet::from_iter(dig.bytes().map(|c| {
                *puzzle_solution
                    .get(&std::str::from_utf8(&[c]).unwrap())
                    .unwrap()
                    .iter()
                    .next()
                    .unwrap()
            })),
        );
    }
    final_mapping
}

impl DisplayPuzzle {
    pub fn untangle(&self) -> HashMap<i32, HashSet<u8>> {
        let mut digit_mapping = HashMap::from([
            ("a", HashSet::from([0, 1, 2, 3, 4, 5, 6])),
            ("b", HashSet::from([0, 1, 2, 3, 4, 5, 6])),
            ("c", HashSet::from([0, 1, 2, 3, 4, 5, 6])),
            ("d", HashSet::from([0, 1, 2, 3, 4, 5, 6])),
            ("e", HashSet::from([0, 1, 2, 3, 4, 5, 6])),
            ("f", HashSet::from([0, 1, 2, 3, 4, 5, 6])),
            ("g", HashSet::from([0, 1, 2, 3, 4, 5, 6])),
        ]);
        for mystery_digit in &self.inputs {
            if mystery_digit.len() == 2 {
                for (key, val) in digit_mapping.iter_mut() {
                    if ["c", "f"].contains(key) {
                        val.retain(|k| mystery_digit.contains(k));
                    } else {
                        val.retain(|k| !mystery_digit.contains(k));
                    }
                }
            } else if mystery_digit.len() == 3 {
                for (key, val) in digit_mapping.iter_mut() {
                    if ["c", "f", "a"].contains(key) {
                        val.retain(|k| mystery_digit.contains(k));
                    } else {
                        val.retain(|k| !mystery_digit.contains(k));
                    }
                }
            } else if mystery_digit.len() == 4 {
                for (key, val) in digit_mapping.iter_mut() {
                    if ["c", "f", "b", "d"].contains(key) {
                        val.retain(|k| mystery_digit.contains(k));
                    } else {
                        val.retain(|k| !mystery_digit.contains(k));
                    }
                }
            }
            if mystery_digit.len() == 5 {
                for (key, val) in digit_mapping.iter_mut() {
                    if ["a", "d", "g"].contains(key) {
                        val.retain(|k| mystery_digit.contains(k));
                    }
                }
            } else if mystery_digit.len() == 6 {
                for (key, val) in digit_mapping.iter_mut() {
                    if ["a", "b", "f", "g"].contains(key) {
                        val.retain(|k| mystery_digit.contains(k));
                    }
                }
            }
        }
        let mut question_keys = HashMap::new();
        for (key_outer, val_outer) in digit_mapping.iter() {
            if val_outer.len() == 2 {
                let mut new_val = val_outer.clone();
                for v in val_outer.iter() {
                    for (key_inner, val_inner) in digit_mapping.iter() {
                        if key_inner != key_outer && val_inner.contains(v) {
                            new_val.retain(|k| k != v);
                        }
                    }
                }
                question_keys.insert(key_outer.clone(), new_val);
            }
        }

        for (key, val) in question_keys.into_iter() {
            digit_mapping.insert(key, val);
        }

        for (key_outer, val_outer) in digit_mapping.iter() {
            assert_eq!(1, val_outer.len());
        }
        let final_mapping = get_final_mapping(&digit_mapping);
        final_mapping
    }
}

impl fmt::Display for DisplayPuzzle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let sol = &self.untangle();
        Ok(for o in &self.outputs {
            for (key, val) in sol.iter() {
                if val == o {
                    write!(f, "{}", key).expect("expect a char");
                }
            }
        })
    }
}

impl FromStr for DisplayPuzzle {
    type Err = Error;
    fn from_str(puzzle_input: &str) -> Result<Self, Self::Err> {
        let mut puzzle_input = puzzle_input.split(" | ");
        Ok(DisplayPuzzle {
            inputs: puzzle_input
                .next()
                .unwrap()
                .split(" ")
                .map(|s| s.bytes().map(|c| c as u8 - 97).collect())
                .collect(),
            outputs: puzzle_input
                .next()
                .unwrap()
                .split(" ")
                .map(|s| s.bytes().map(|c| c as u8 - 97).collect())
                .collect(),
        })
    }
}

fn read<R: Read>(io: R) -> Result<Vec<DisplayPuzzle>, Error> {
    Ok(BufReader::new(io)
        .lines()
        .map(|line| line.unwrap().trim().parse().unwrap())
        .collect())
}

pub fn day08() {
    let display_observation = read(File::open("inputs/day08.txt").unwrap()).unwrap();

    for p in display_observation.iter() {
        let yada = format!("{}", p);
        println!("puzzle_input: {:?}", yada.parse::<i32>().unwrap());
    }

    let final_ans: i32 = display_observation.iter().map(|d| format!("{}", d).parse::<i32>().unwrap()).sum();
    println!("puzzle_input: {:?}", final_ans);

}

