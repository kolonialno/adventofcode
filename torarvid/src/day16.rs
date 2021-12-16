use std::str::Chars;

use crate::util::file_by_lines;

pub fn run() {
    let raw = file_by_lines("day16.txt").into_iter().next().unwrap();
    let input = Input { raw };
    let mut iter = input.iter();
    let packet = iter.to_packet();
    println!("Part 1: {:?}", packet.sum_versions());
    println!("Part 2: {:?}", packet.evaluate());
}

struct Input {
    raw: String,
}

impl Input {
    fn iter(&self) -> InputIterator {
        InputIterator {
            chars: self.raw.chars(),
            current_pos: 0,
            current: None,
            pos: 0,
            len: self.raw.len(),
        }
    }
}

#[derive(Debug)]
struct BitString(Vec<bool>);

impl From<BitString> for Input {
    fn from(bits: BitString) -> Self {
        let fold = |acc, bit: &bool| acc * 2 | if *bit { 1 } else { 0 };
        let raw = bits
            .0
            .chunks(4)
            .map(|bits| {
                // pad bits with false on the right if length is < 4
                let mut padded = bits.to_vec();
                while padded.len() < 4 {
                    padded.push(false);
                }
                padded
            })
            .map(|bits| bits.iter().fold(0, fold))
            .map(|v| format!("{:X}", v))
            .collect();
        Input { raw }
    }
}

struct InputIterator<'a> {
    chars: Chars<'a>,
    current_pos: u8,
    current: Option<char>,
    pos: usize,
    len: usize,
}

impl<'a> InputIterator<'a> {
    fn get_bits(&mut self, num_bits: usize) -> BitString {
        let mut bits = match self.current {
            Some(c) => self
                .char_to_bits(c)
                .into_iter()
                .skip(self.current_pos as usize)
                .collect::<Vec<_>>(),
            None => Vec::new(),
        };
        while bits.len() < num_bits {
            self.pos += 1;
            if self.pos > self.len {
                panic!("ran out of input");
            }
            self.current = self.chars.next();
            bits.extend(self.char_to_bits(self.current.unwrap()));
        }
        self.current_pos = ((self.current_pos as usize + num_bits) % 4) as u8;
        if self.current_pos == 0 {
            self.current = None;
        }
        BitString(bits.iter().take(num_bits).cloned().collect())
    }

    fn char_to_bits(&self, c: char) -> [bool; 4] {
        let num: u8 = c.to_digit(16).unwrap().try_into().unwrap();
        let mut bits = [false; 4];
        for i in 0..4 {
            bits[i] = (num & (1 << (3 - i))) != 0;
        }
        bits
    }

    fn get_bits_as_u64(&mut self, num_bits: usize) -> u64 {
        let bits = self.get_bits(num_bits);
        InputIterator::bits_to_u64(bits.0)
    }

    fn bits_to_u64(bits: Vec<bool>) -> u64 {
        let mut num = 0;
        for (i, bit) in bits.iter().enumerate() {
            if *bit {
                num |= 1 << (bits.len() - 1 - i);
            }
        }
        num
    }

    fn remaining_bits(&self) -> usize {
        let bits_from_remaining_chars = (self.len - self.pos) * 4;
        if self.current.is_some() {
            bits_from_remaining_chars + 4 - self.current_pos as usize
        } else {
            bits_from_remaining_chars
        }
    }

    fn to_packet(&mut self) -> Packet {
        let version = self.get_bits_as_u64(3) as u8;
        let type_id = self.get_bits_as_u64(3) as u8;
        let packet_type = match type_id {
            0 => Type::Operator(Op::Sum),
            1 => Type::Operator(Op::Product),
            2 => Type::Operator(Op::Min),
            3 => Type::Operator(Op::Max),
            4 => Type::Literal,
            5 => Type::Operator(Op::GreaterThan),
            6 => Type::Operator(Op::LessThan),
            7 => Type::Operator(Op::Equal),
            _ => panic!("unknown packet type"),
        };
        let mut content: Option<u64> = None;
        let mut sub_packets: Vec<Packet> = Vec::new();
        match packet_type {
            Type::Operator(_) => {
                let length_type_id = self.get_bits_as_u64(1);
                let length_type = match length_type_id {
                    0 => LengthType::BitLength(self.get_bits_as_u64(15) as u16),
                    1 => LengthType::PacketCount(self.get_bits_as_u64(11) as u16),
                    _ => panic!("invalid length type id"),
                };
                sub_packets.extend(match length_type {
                    LengthType::BitLength(length) => self.n_bits_to_subpackets(length.into()),
                    LengthType::PacketCount(count) => self.to_n_subpackets(count.into()),
                });
            }
            Type::Literal => {
                let mut bits = Vec::new();
                loop {
                    let raw = self.get_bits(5).0;
                    let cont = raw[0];
                    let number = &raw[1..=4];
                    bits.extend(number);
                    if !cont {
                        break;
                    }
                }
                content = Some(InputIterator::bits_to_u64(bits));
            }
        }
        Packet {
            version,
            packet_type,
            content,
            sub_packets,
        }
    }

    fn consume_packets(&mut self) -> Vec<Packet> {
        let mut packets = Vec::new();
        // packets are always at least 6 bits long
        while self.remaining_bits() >= 6 {
            packets.push(self.to_packet());
        }
        packets
    }

    fn to_n_subpackets(&mut self, n: usize) -> Vec<Packet> {
        let mut packets = Vec::new();
        for _ in 0..n {
            packets.push(self.to_packet());
        }
        packets
    }

    fn n_bits_to_subpackets(&mut self, n: usize) -> Vec<Packet> {
        let bits = self.get_bits(n);
        Input::from(bits).iter().consume_packets()
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Op {
    Sum,
    Product,
    Min,
    Max,
    GreaterThan,
    LessThan,
    Equal,
}

#[derive(Debug)]
enum Type {
    Literal,
    Operator(Op),
}

#[derive(Debug, Clone)]
enum LengthType {
    BitLength(u16),
    PacketCount(u16),
}

#[derive(Debug)]
struct Packet {
    version: u8,
    packet_type: Type,
    content: Option<u64>,
    sub_packets: Vec<Packet>,
}

impl Packet {
    fn sum_versions(&self) -> u64 {
        let mut sum = self.version as u64;
        for packet in &self.sub_packets {
            sum += packet.sum_versions();
        }
        sum
    }

    fn evaluate(&self) -> u64 {
        let subs = &self.sub_packets;
        match self.packet_type {
            Type::Literal => self.content.unwrap(),
            Type::Operator(op) => match op {
                Op::Sum => subs.iter().map(|p| p.evaluate()).sum(),
                Op::Product => subs.iter().map(|p| p.evaluate()).product(),
                Op::Min => subs.iter().map(|p| p.evaluate()).min().unwrap(),
                Op::Max => subs.iter().map(|p| p.evaluate()).max().unwrap(),
                Op::GreaterThan => {
                    if subs[0].evaluate() > subs[1].evaluate() {
                        1
                    } else {
                        0
                    }
                }
                Op::LessThan => {
                    if subs[0].evaluate() < subs[1].evaluate() {
                        1
                    } else {
                        0
                    }
                }
                Op::Equal => {
                    if subs[0].evaluate() == subs[1].evaluate() {
                        1
                    } else {
                        0
                    }
                }
            },
        }
    }
}
