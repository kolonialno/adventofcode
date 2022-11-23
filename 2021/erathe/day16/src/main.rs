use core::panic;

use itertools::Itertools;

fn main() {
    let input = include_str!("../input.txt")
        .chars()
        .map(to_binary)
        .collect_vec()
        .join("");

    println!("{}", part1(input.as_str()));
    println!("{}", part2(input.as_str()));
}

fn part2(input: &str) -> i64 {
    let res = rec(input);
    res.0
}

fn rec(packets: &str) -> (i64, i64, usize) {
    let t_i = val_10(&packets[3..6]);
    let mut val = match t_i {
        1 => 1,
        2 => i64::MAX,
        _ => 0,
    };
    let mut t = Vec::new();

    if t_i == 4 {
        let (value, bytes_parsed) = parse_literal_value(&packets[6..]);
        return (value, 1, 6 + bytes_parsed);
    }

    let (p_to_parse, b_to_parse, mut b_parsed, mut p_parsed) = match &packets[6..7].as_bytes() {
        &b"1" => (val_10(&packets[7..18]), usize::MAX, 18, 0),
        _ => (i64::MAX, val_10(&packets[7..22]) as usize + 22, 22, 0),
    };

    while b_parsed < b_to_parse && p_parsed < p_to_parse {
        let (v, p_p, b_p) = rec(&packets[b_parsed..]);
        p_parsed += p_p;
        b_parsed += b_p;
        match t_i {
            0 => val += v,
            1 => val *= v,
            2 => val = val.min(v),
            3 => val = val.max(v),
            5 => {
                t.push(v);
                val = if t.len() == 2 && t[0] > t[1] { 1 } else { 0 };
            }
            6 => {
                t.push(v);
                val = if t.len() == 2 && t[0] < t[1] { 1 } else { 0 };
            }
            7 => {
                t.push(v);
                val = if t.len() == 2 && t[0] == t[1] { 1 } else { 0 };
            }
            _ => panic!("shouldn't happen"),
        };
    }
    (val, 1, b_parsed)
}

// really naive solution that does not work at all for part 2
fn part1(input: &str) -> i64 {
    let mut version = 0;
    let mut ptr = 0;
    loop {
        // no bits to process
        if input[ptr..].chars().all(|c| c == '0') {
            break;
        }
        match val_10(&input[ptr + 3..ptr + 6]) {
            4 => {
                version += val_10(&input[ptr..ptr + 3]);
                ptr += 6;
                let bytes_parsed = parse_literal_value(&input[ptr..]).1;
                ptr += bytes_parsed;
            }
            _ => {
                version += val_10(&input[ptr..ptr + 3]);
                ptr += 6;
                // just skip these for now
                if &input[ptr..ptr + 1].as_bytes() == b"0" {
                    ptr += 16
                } else {
                    ptr += 12
                }
            }
        }
    }
    version
}

fn val_10(v: &str) -> i64 {
    i64::from_str_radix(v, 2).unwrap()
}

fn parse_literal_value(literal: &str) -> (i64, usize) {
    let mut res_str = String::new();
    let mut i = 0;
    loop {
        let is_last = &literal[i..i + 1].as_bytes() == b"0";
        res_str.push_str(&literal[i + 1..i + 5]);
        i += 5;
        if is_last {
            break;
        }
    }

    (val_10(res_str.as_str()), i)
}

fn to_binary(c: char) -> &'static str {
    match c {
        '0' => "0000",
        '1' => "0001",
        '2' => "0010",
        '3' => "0011",
        '4' => "0100",
        '5' => "0101",
        '6' => "0110",
        '7' => "0111",
        '8' => "1000",
        '9' => "1001",
        'A' => "1010",
        'B' => "1011",
        'C' => "1100",
        'D' => "1101",
        'E' => "1110",
        'F' => "1111",
        _ => panic!("could not parse char: {}", c),
    }
}
