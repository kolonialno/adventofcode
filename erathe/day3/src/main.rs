use itertools::Itertools;

fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(input));
    println!("{}", part2(input));
}

fn part2(input: &str) -> isize {
    let lines = input.lines().collect::<Vec<&str>>();

    get_binary_value(&lines, &larger) * get_binary_value(&lines, &smaller)
}

fn larger(b: i32) -> bool {
    b >= 0
}
fn smaller(b: i32) -> bool {
    b < 0
}

fn get_binary_value(lines: &Vec<&str>, pred: &dyn Fn(i32) -> bool) -> isize {
    let mut w_lines = lines.clone();
    for i in 0..12 {
        let bits = w_lines.iter().clone().fold(0, |acc, s| {
            if s.as_bytes()[i] as u8 == b'1' {
                return acc + 1;
            } else {
                acc - 1
            }
        });

        let important_bit = if pred(bits) { b'1' } else { b'0' };

        let new_lines = w_lines
            .clone()
            .into_iter()
            .filter(|s| s.as_bytes()[i] == important_bit)
            .collect_vec();

        if new_lines.len() == 1 {
            return isize::from_str_radix(&new_lines[0], 2).unwrap();
        }
        w_lines = new_lines
    }
    0
}

fn part1(input: &str) -> isize {
    let len = input.lines().count();
    let mut occs: [u32; 12] = [0; 12];
    let mut gamma: [char; 12] = ['0'; 12];
    let mut epsilon: [char; 12] = ['0'; 12];

    input.lines().for_each(|b_str| {
        for (idx, c) in b_str
            .as_bytes()
            .iter()
            .enumerate()
            .collect::<Vec<(usize, &u8)>>()
        {
            if *c == b'1' {
                occs[idx] += 1
            };
        }
    });

    for n in 0..12 {
        if occs[n] > len as u32 / 2 {
            gamma[n] = '1';
        } else {
            epsilon[n] = '1';
        }
    }
    let gamma_str = String::from_iter(gamma);
    let epsi_str = String::from_iter(epsilon);

    let p1 = isize::from_str_radix(&gamma_str, 2).unwrap()
        * isize::from_str_radix(&epsi_str, 2).unwrap();

    p1
}
