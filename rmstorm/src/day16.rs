use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

const DEBUG_PRINTING: bool = false;

fn read<R: Read>(io: R) -> Result<Vec<u8>, Error> {
    let mut br = BufReader::new(io);
    let mut chars = String::new();
    br.read_line(&mut chars).unwrap();
    let chars = chars
        .trim()
        .chars()
        .map(|i| u8::from_str_radix(&String::from(i), 16).unwrap())
        .collect();

    Ok(chars)
}

#[derive(Debug, PartialEq, Eq)]
struct Packet {
    version: u32,
    typeid: u32,
    sub_packets: Vec<Packet>,
    length: usize,
    value: Option<u64>,
}

fn parse_slice(packet: &[char]) -> u32 {
    let char_string: String = packet.iter().collect();
    u32::from_str_radix(&char_string, 2).unwrap()
}

fn parse_concrete_packet(mut packet: Packet, packet_bits: &[char]) -> Packet {
    let mut i = 6;
    let mut reading = true;
    let mut packet_value: Vec<char> = vec![];
    while reading {
        packet_value.extend(&packet_bits[i + 1..i + 5]);
        reading = packet_bits[i] != '0'; // stop reading when a 0 bit is encountered
        i += 5;
    }
    let packet_value: String = packet_value.iter().collect();
    packet.value = Some(u64::from_str_radix(&packet_value, 2).unwrap());
    packet.length = i;
    if DEBUG_PRINTING {
        for i in 6..packet.length {
            print!("\u{001b}[35m{}\u{001b}[0m", packet_bits[i]);
        }
    }
    packet
}

fn parse_subpackets(mut packet: Packet, packet_bits: &[char]) -> Packet {
    let length_type_id = parse_slice(&packet_bits[6..7]);
    if DEBUG_PRINTING {
        for i in 6..7 {
            print!("\u{001b}[33m{}\u{001b}[0m", packet_bits[i]);
        }
    }
    if length_type_id == 0 {
        let subpackets_length = parse_slice(&packet_bits[7..22]);
        let mut cur_packet_start = 22;
        if DEBUG_PRINTING {
            for i in 7..22 {
                print!("\u{001b}[34m{}\u{001b}[0m", packet_bits[i]);
            }
        }
        while cur_packet_start != (subpackets_length + 22) as usize {
            let sub_packet = parse_packet(&packet_bits[cur_packet_start..packet_bits.len()]);
            cur_packet_start += sub_packet.length;
            packet.sub_packets.push(sub_packet);
        }
        packet.length = cur_packet_start
    } else {
        let mut subpackets = parse_slice(&packet_bits[7..18]);
        let mut cur_packet_start = 18;
        if DEBUG_PRINTING {
            for i in 7..18 {
                print!("\u{001b}[34m{}\u{001b}[0m", packet_bits[i]);
            }
        }
        while subpackets > 0 {
            let sub_packet = parse_packet(&packet_bits[cur_packet_start..packet_bits.len()]);
            cur_packet_start += sub_packet.length;
            packet.sub_packets.push(sub_packet);
            subpackets -= 1;
        }
        packet.length = cur_packet_start
    }
    packet
}

fn parse_packet(packet_bits: &[char]) -> Packet {
    let mut packet = Packet {
        version: parse_slice(&packet_bits[0..3]),
        typeid: parse_slice(&packet_bits[3..6]),
        sub_packets: vec![],
        length: packet_bits.len(),
        value: None,
    };
    if DEBUG_PRINTING {
        for i in 0..3 {
            print!("\u{001b}[31m{}\u{001b}[0m", packet_bits[i]);
        }
        for i in 3..6 {
            print!("\u{001b}[32m{}\u{001b}[0m", packet_bits[i]);
        }
    }
    if packet.typeid == 4 {
        packet = parse_concrete_packet(packet, packet_bits);
    } else {
        packet = parse_subpackets(packet, packet_bits);
    }
    return packet;
}

fn sum_versions(packet: &Packet, mut version_sum: &mut u32) {
    *version_sum += packet.version;
    for sub_packet in packet.sub_packets.iter() {
        sum_versions(sub_packet, &mut version_sum);
    }
}

fn resolve_value(mut packet: &mut Packet) -> u64 {
    if packet.typeid == 0 {
        let mut final_sum = 0;
        for mut sp in packet.sub_packets.iter_mut() {
            final_sum += resolve_value(&mut sp);
        }
        packet.value = Some(final_sum);
    } else if packet.typeid == 1 {
        let mut final_product = 1;
        for mut sp in packet.sub_packets.iter_mut() {
            final_product *= resolve_value(&mut sp);
        }
        packet.value = Some(final_product);
    } else if packet.typeid == 2 {
        let mut number_array = vec![];
        for mut sp in packet.sub_packets.iter_mut() {
            number_array.push(resolve_value(&mut sp));
        }
        packet.value = Some(*number_array.iter().min().unwrap());
    } else if packet.typeid == 3 {
        let mut number_array = vec![];
        for mut sp in packet.sub_packets.iter_mut() {
            number_array.push(resolve_value(&mut sp));
        }
        packet.value = Some(*number_array.iter().max().unwrap());
    } else if packet.typeid == 5 {
        packet.value = Some(
            (resolve_value(&mut packet.sub_packets[0]) > resolve_value(&mut packet.sub_packets[1]))
                as u64,
        );
    } else if packet.typeid == 6 {
        packet.value = Some(
            (resolve_value(&mut packet.sub_packets[0]) < resolve_value(&mut packet.sub_packets[1]))
                as u64,
        );
    } else if packet.typeid == 7 {
        packet.value = Some(
            (resolve_value(&mut packet.sub_packets[0]) == resolve_value(&mut packet.sub_packets[1]))
                as u64,
        );
    }
    packet.value.unwrap()
}

pub fn day16() {
    let chars = read(File::open("inputs/day16.txt").unwrap()).unwrap();
    let mut v: Vec<char> = vec![];

    for c in chars {
        v.extend(format!("{:04b}", c).chars());
    }
    println!("number of bits: {:?}", v.len());
    if DEBUG_PRINTING {
        for v in v.iter() {
            print!("{}", v);
        }
        print!("\n");
    }
    let mut parsed_packet = parse_packet(&v);
    print!("\n");
    let mut version_sum = 0;
    sum_versions(&parsed_packet, &mut version_sum);
    println!("version_sum {:?}", version_sum);

    println!("interpreted answer {:?}", resolve_value(&mut parsed_packet));
}
