use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

fn read<R: Read>(io: R) -> Result<(Vec<u16>, usize), Error> {
    let mut br = BufReader::new(io);
    let mut v = vec![];

    // Read one line manually to determine length...
    let mut first_line = String::new();
    br.read_line(&mut first_line).unwrap();
    let first_line = first_line.trim();
    v.push(u16::from_str_radix(&first_line, 2).unwrap());

    // Read the rest of the lines
    for line in br.lines() {
        v.push(u16::from_str_radix(line?.trim(), 2).unwrap());
    }
    Ok((v, first_line.len()))
}

fn retain_nums(mut new_readouts: Vec<u16>, check_bytes: &Vec<u16>, long: bool) -> u16 {
    for cb in check_bytes {
        let zero_indices: Vec<bool> = (0..new_readouts.len())
            .map(|i| (new_readouts[i] & cb) == 0)
            .collect();
        let num_zeros = zero_indices.iter().filter(|&b| *b).count();

        let zeros_won = (num_zeros * 2) > new_readouts.len();
        let retain = long ^ zeros_won;

        let mut keep_iter = zero_indices.iter();
        new_readouts.retain(|_| (retain as bool) ^ *keep_iter.next().unwrap());

        if new_readouts.len() == 1 {
            break;
        }
    }
    new_readouts[0]
}

pub fn day03() {
    let (readouts, line_length) = read(File::open("inputs/day03.txt").unwrap()).unwrap();
    let num_readouts = readouts.len() as u16;

    // make a vector of numbers that have bits set on the diagonal like so:
    // [100
    //  010
    //  001] they're just powers of 2 starting with the most significant bit!
    let check_bytes: Vec<u16> = (0..line_length)
        .rev()
        .map(|i| 2_u16.pow(i as u32))
        .collect();

    let mut least_common_bit_num: u16 = 0;
    for cb in &check_bytes {
        let count_zeroes = &readouts
            .iter()
            .fold(0, |accum, r| accum + ((r & cb) == 0) as u16);
        least_common_bit_num |= cb * (count_zeroes * 2 > num_readouts) as u16;
    }
    let most_common_bit_num = least_common_bit_num ^ (2_u16.pow(line_length as u32) - 1_u16);
    let ans1 = most_common_bit_num as u32 * least_common_bit_num as u32;
    println!("answer 1 = {:#?}", ans1);

    
    let most_common_bit_num = retain_nums(readouts.clone(), &check_bytes, true);
    let least_common_bit_num = retain_nums(readouts.clone(), &check_bytes, false);
    let ans2 = most_common_bit_num as u32 * least_common_bit_num as u32;
    println!("answer 2 = {:#?}", ans2);
    // println!("0b{:0width$b}", least_common_bit_num, width = line_length);
    // println!("{}", least_common_bit_num);
}
