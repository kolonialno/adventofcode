fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(input));
    println!("{}", part2(input));
}

fn part2(input: &str) -> u32 {
    let mut tot = 0;
    let mut bags: [&str; 3] = [""; 3];
    let mut idx = 0;
    for s in input.lines() {
        bags[idx] = s;
        if idx == 2 {
            for c in bags[0].chars() {
                if bags[1].contains(c) && bags[2].contains(c) {
                    tot += c.to_scuffed_ascii();
                    break;
                }
            }
            idx = 0;
            continue;
        }
        idx += 1;
    }
    tot
}

fn part1(input: &str) -> u32 {
    input
        .lines()
        .map(|l| {
            let (one, two) = l.split_at(l.len() / 2);
            let mut r = '_';
            for c in one.chars() {
                if two.contains(c) {
                    r = c;
                    break;
                }
            }
            r.to_scuffed_ascii()
        })
        .sum::<u32>()
}

pub trait ScuffedAscii {
    fn to_scuffed_ascii(&self) -> u32;
}

impl ScuffedAscii for char {
    fn to_scuffed_ascii(&self) -> u32 {
        let mut r = u32::from(*self);
        if r > 96 {
            r -= 96
        } else {
            r -= 38
        }
        r
    }
}
