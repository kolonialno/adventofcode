use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};
use std::ops;

#[derive(Debug, PartialEq, Eq, Clone)]
struct Cuboid {
    coords: [[isize; 2]; 3],
    on: bool,
}

fn coord_overlap(c1: &[isize; 2], c2: &[isize; 2]) -> [isize; 2] {
    let mut c3 = [0, 0];

    if c1[0] <= c2[0] && c2[0] < c1[1] {
        if c1[0] < c2[1] && c2[1] <= c1[1] {
            c3 = [c2[0], c2[1]]
        } else {
            c3 = [c2[0], c1[1]]
        }
    } else if c2[0] <= c1[0] && c1[0] < c2[1] {
        if c2[0] < c1[1] && c1[1] <= c2[1] {
            c3 = [c1[0], c1[1]]
        } else {
            c3 = [c1[0], c2[1]]
        }
    }
    c3
}

impl Cuboid {
    fn size(&self) -> isize {
        let mut size = 1;
        for c in self.coords.iter() {
            size *= c[1] - c[0]
        }
        return size;
    }
    fn overlap(&self, other: &Cuboid) -> Option<Cuboid> {
        let coords: Vec<[isize; 2]> = self
            .coords
            .iter()
            .zip(other.coords.iter())
            .map(|(c1, c2)| coord_overlap(c1, c2))
            .collect();
        let newc = Cuboid {
            coords: coords.try_into().unwrap(),
            on: true,
        };
        if newc.size() > 0 {
            Some(newc)
        } else {
            None
        }
    }
}

fn sub_cube_coords(c1: &[isize; 2], c2: &[isize; 2]) -> Vec<[isize; 2]> {
    let mut sub_coords = vec![];
    if c1[0] <= c2[0] && c2[0] < c1[1] {
        sub_coords.push([c1[0], c2[0]]);
        if c1[0] < c2[1] && c2[1] <= c1[1] {
            sub_coords.push([c2[0], c2[1]]);
            sub_coords.push([c2[1], c1[1]]);
        } else {
            sub_coords.push([c2[0], c1[1]]);
        }
    } else if c2[0] <= c1[0] && c1[0] < c2[1] {
        if c2[0] < c1[1] && c1[1] <= c2[1] {
            sub_coords.push([c1[0], c1[1]]);
        } else {
            sub_coords.push([c1[0], c2[1]]);
            sub_coords.push([c2[1], c1[1]]);
        }
    }
    sub_coords
}

impl ops::Sub<&Cuboid> for &Cuboid {
    type Output = Vec<Cuboid>;

    fn sub(self, rhs: &Cuboid) -> Vec<Cuboid> {
        let x_coords = sub_cube_coords(&self.coords[0], &rhs.coords[0]);
        let y_coords = sub_cube_coords(&self.coords[1], &rhs.coords[1]);
        let z_coords = sub_cube_coords(&self.coords[2], &rhs.coords[2]);

        let mut cuboids_out = vec![];
        for x in x_coords.iter() {
            for y in y_coords.iter() {
                for z in z_coords.iter() {
                    let new_cube = Cuboid {
                        coords: [*x, *y, *z],
                        on: true,
                    };
                    if new_cube.size() > 0 && new_cube.overlap(rhs) == None {
                        cuboids_out.push(new_cube)
                    }
                }
            }
        }
        cuboids_out
    }
}

fn get_range(r: &str) -> [isize; 2] {
    let mut positions = r[2..r.len()].split("..");
    [
        positions.next().unwrap().parse::<isize>().unwrap(),
        positions.next().unwrap().parse::<isize>().unwrap() + 1,
    ]
}

fn make_cuboid(step_ranges: &str, on: bool) -> Cuboid {
    let coords: Vec<[isize; 2]> = step_ranges.split(",").map(|r| get_range(r)).collect();
    Cuboid {
        coords: coords.try_into().unwrap(),
        on,
    }
}

fn read<R: Read>(io: R) -> Result<Vec<Cuboid>, Error> {
    let mut steps = vec![];
    for line in BufReader::new(io).lines() {
        let line = line.unwrap();
        match &line.trim()[1..2] {
            "n" => {
                steps.push(make_cuboid(&line[3..line.len()], true));
            }
            _ => {
                steps.push(make_cuboid(&line[4..line.len()], false));
            }
        }
    }
    Ok(steps)
}

fn filter_addables(new_cube: Cuboid, addables: &mut Vec<Cuboid>, final_cubes: &Vec<Cuboid>) {
    for f in final_cubes {
        if let Some(overlap_cube) = f.overlap(&new_cube) {
            for c in &new_cube - &overlap_cube {
                filter_addables(c, addables, final_cubes)
            }
            return;
        }
    }
    addables.push(new_cube)
}

fn add_to_final(final_cubes: &mut Vec<Cuboid>, new_cube: &Cuboid) {
    let mut addables = vec![];
    filter_addables(new_cube.clone(), &mut addables, final_cubes);
    final_cubes.extend(addables);
}

fn remove_from_final(final_cubes: &mut Vec<Cuboid>, new_cube: &Cuboid) {
    let mut addables = vec![];
    let mut removables = vec![];
    for (index, final_cube) in final_cubes.iter().enumerate() {
        if let Some(_) = new_cube.overlap(final_cube) {
            removables.push(index);
            let leftovers = final_cube - new_cube;
            addables.extend(leftovers);
        }
    }
    removables.sort();
    for r in removables.iter().rev() {
        final_cubes.swap_remove(*r);
    }
    final_cubes.extend(addables);
}

pub fn day22() {
    let steps = read(File::open("inputs/day22.txt").unwrap()).unwrap();
    let mut final_cubes = vec![];

    let mut i: u32 = 0;
    for s in steps.iter() {
        i += 1;
        if s.on {
            add_to_final(&mut final_cubes, s);
        } else {
            remove_from_final(&mut final_cubes, s)
        }
        println!("a {} {:?}", i, final_cubes.len());
        println!("{:?}", final_cubes.iter().map(|c| c.size()).sum::<isize>());
    }
}
