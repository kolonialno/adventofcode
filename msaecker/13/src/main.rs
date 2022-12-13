use std::{cmp::Ordering, fs, str::FromStr};

use helpers::get_input_file;

#[derive(Clone, Debug)]
enum Packet {
    List(Vec<Packet>),
    Item(isize),
}

impl FromStr for Packet {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut packets = Vec::default();
        let mut open_brackets = 0;
        let mut comma = 0;
        if s.chars().next().unwrap().is_numeric() {
            return Ok(Packet::Item(s.parse::<isize>().unwrap()));
        }
        for (idx, c) in s.char_indices() {
            if c == '[' {
                // start new list
                open_brackets += 1;
            } else if c == ']' {
                // end list
                open_brackets -= 1;
                if open_brackets == 0 {
                    // end last open list
                    let segment = s.chars().take(idx).skip(comma + 1).collect::<String>();
                    if !segment.is_empty() {
                        packets.push(Self::from_str(&segment).unwrap());
                    }
                }
            } else if c == ',' {
                // element done
                if open_brackets == 1 {
                    // innermost list
                    let segment = s.chars().take(idx).skip(comma + 1).collect::<String>();
                    packets.push(Self::from_str(&segment).unwrap());
                    comma = idx;
                }
            }
        }
        Ok(Packet::List(packets))
    }
}

impl Eq for Packet {}

impl PartialEq for Packet {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::List(l0), Self::List(r0)) => {
                if l0.len() != r0.len() {
                    return false;
                }
                for (lhs, rhs) in l0.iter().zip(r0.iter()) {
                    if lhs != rhs {
                        return false;
                    }
                }
                true
            }
            (Self::Item(l0), Self::Item(r0)) => l0 == r0,
            (Packet::List(_), Packet::Item(_)) => *self == Packet::List(vec![other.clone()]),
            (Packet::Item(_), Packet::List(_)) => Packet::List(vec![self.clone()]) == *other,
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::List(l0), Self::List(r0)) => {
                for (lhs, rhs) in l0.iter().zip(r0.iter()) {
                    match lhs.partial_cmp(rhs).unwrap() {
                        Ordering::Less => return Some(Ordering::Less),
                        Ordering::Equal => {}
                        Ordering::Greater => return Some(Ordering::Greater),
                    }
                }
                l0.len().partial_cmp(&r0.len())
            }
            (Self::Item(l0), Self::Item(r0)) => l0.partial_cmp(r0),
            (Packet::List(_), Packet::Item(_)) => {
                self.partial_cmp(&Packet::List(vec![other.clone()]))
            }
            (Packet::Item(_), Packet::List(_)) => {
                Packet::List(vec![self.clone()]).partial_cmp(other)
            }
        }
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

fn main() {
    // let input = fs::read_to_string("13/test.txt").unwrap();
    let input = fs::read_to_string(get_input_file()).unwrap();
    let mut correct_packets = Vec::default();
    let mut packets = Vec::default();
    for (idx, pair) in input.split("\n\n").enumerate() {
        let (lhs_pair, rhs_pair) = pair.split_once('\n').unwrap();
        let lhs = Packet::from_str(lhs_pair).unwrap();
        let rhs = Packet::from_str(rhs_pair).unwrap();

        match lhs.partial_cmp(&rhs).unwrap() {
            Ordering::Less | Ordering::Equal => correct_packets.push(idx + 1),
            Ordering::Greater => {}
        }
        packets.push(lhs);
        packets.push(rhs);
    }

    let divider1 = Packet::from_str("[[2]]").unwrap();
    let divider2 = Packet::from_str("[[6]]").unwrap();
    packets.push(divider1.clone());
    packets.push(divider2.clone());
    packets.sort();

    let div1_pos = packets.iter().position(|x| *x == divider1).unwrap() + 1;
    let div2_pos = packets.iter().position(|x| *x == divider2).unwrap() + 1;

    println!(
        "Sum of correct packets: {}",
        correct_packets.iter().sum::<usize>()
    );
    println!("Decoder key : {}", div1_pos * div2_pos);
}
