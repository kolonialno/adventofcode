use ndarray::{arr1, arr2, Array1, Array2};
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

#[derive(Debug, PartialEq, Eq, Clone)]
struct Scanner {
    pos: Array1<i32>,
    orientation: Array2<i32>,
    beacons: Vec<Array1<i32>>,
}

impl Scanner {
    fn new() -> Scanner {
        Scanner {
            pos: arr1(&[0, 0, 0]),
            orientation: arr2(&[[1, 0, 0], [0, 1, 0], [0, 0, 1]]),
            beacons: vec![],
        }
    }
    fn reorient(&mut self, new_orientation: &Array2<i32>) {
        self.orientation = self.orientation.dot(new_orientation);
        for i in 0..self.beacons.len() {
            self.beacons[i] = new_orientation.dot(&self.beacons[i]);
        }
    }
}

fn read<R: Read>(io: R) -> Result<Vec<Scanner>, Error> {
    let mut inputs = vec![];
    let mut scanner = Scanner::new();
    for line in BufReader::new(io).lines() {
        let line = line.unwrap();
        if line != "" {
            match &line.trim()[1..2] {
                "-" => {
                    if scanner.beacons.len() != 0 {
                        inputs.push(scanner);
                        scanner = Scanner::new();
                    }
                }
                _ => {
                    scanner.beacons.push(
                        line.split(",")
                            .map(|n| n.parse().unwrap())
                            .collect::<Vec<i32>>()
                            .try_into()
                            .unwrap(),
                    );
                }
            }
        }
    }
    inputs.push(scanner);

    Ok(inputs)
}

fn make_orientations() -> Vec<Array2<i32>> {
    let x_rots = [
        arr2(&[[1, 0, 0], [0, 1, 0], [0, 0, 1]]),
        arr2(&[[0, -1, 0], [1, 0, 0], [0, 0, 1]]),
        arr2(&[[-1, 0, 0], [0, -1, 0], [0, 0, 1]]),
        arr2(&[[0, 1, 0], [-1, 0, 0], [0, 0, 1]]),
    ];
    let views = [
        arr2(&[[1, 0, 0], [0, 1, 0], [0, 0, 1]]),
        arr2(&[[-1, 0, 0], [0, 1, 0], [0, 0, -1]]),
        arr2(&[[0, 0, 1], [0, 1, 0], [-1, 0, 0]]),
        arr2(&[[0, 0, -1], [0, 1, 0], [1, 0, 0]]),
        arr2(&[[1, 0, 0], [0, 0, -1], [0, 1, 0]]),
        arr2(&[[1, 0, 0], [0, 0, 1], [0, -1, 0]]),
    ];
    let mut orientations = vec![];
    for v in views.iter() {
        for vv in x_rots.iter() {
            orientations.push(v.dot(vv))
        }
    }
    orientations
}

fn matchup(ors: &Vec<Array2<i32>>, scanners: &mut Vec<Scanner>, s1: usize, s2: usize) -> bool {
    println!("Searching for {} from {}", s2, s1);
    for o in ors.iter() {
        scanners[s2].reorient(o);
        let mut frequency: HashMap<Array1<i32>, u32> = HashMap::new();
        for b1 in scanners[s1].beacons.iter() {
            for b2 in scanners[s2].beacons.iter() {
                *frequency.entry(b1 - b2).or_insert(0) += 1;
                if frequency.get(&(b1 - b2)).unwrap() > &11 {
                    println!("found overlap between {} and {} at {:?}", s1, s2, b1 - b2);
                    scanners[s2].pos = &scanners[s1].pos + (b1 - b2);
                    return true;
                }
            }
        }
        scanners[s2].reorient(&o.t().to_owned());
    }
    false
}

fn search_from(
    ors: &Vec<Array2<i32>>,
    scanners: &mut Vec<Scanner>,
    from: usize,
    found: &mut Vec<usize>,
) {
    let mut new_found = vec![];

    for i in 0..scanners.len() {
        if !found.contains(&i) {
            if matchup(ors, scanners, from, i) {
                new_found.push(i);
            }
        }
    }
    found.extend(new_found.clone());
    for num in new_found {
        search_from(ors, scanners, num, found)
    }
}

pub fn day19() {
    let mut scanners = read(File::open("inputs/day19.txt").unwrap()).unwrap();
    let orientations = make_orientations();

    let mut found = vec![0];
    search_from(&orientations, &mut scanners, 0, &mut found);

    let mut all_beacons = HashSet::new();
    for s in scanners.iter() {
        for b in s.beacons.iter() {
            all_beacons.insert(&s.pos + b);
        }
    }
    println!("answer1: {:?}", all_beacons.len());

    let mut max_distance = 0;
    for i in 0..scanners.len() {
        for ii in 0..scanners.len() {
            max_distance = max_distance.max(
                (&scanners[i].pos - &scanners[ii].pos)
                    .iter()
                    .map(|a| a.abs())
                    .sum(),
            );
        }
    }
    println!("answer2: {:?}", max_distance);
}
