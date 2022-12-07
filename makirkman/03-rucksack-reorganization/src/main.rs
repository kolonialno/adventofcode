use std::{
    fs::File,
    io::{BufRead, BufReader},
    collections::HashSet,
};

mod helpers;

// lowercase a-z have priorities 1-26 ; ASCII values 97-122
const L_PRIORITY: u32 = 1;
const L_ASCII: u32 = 97;
// uppercase A-Z have priorities 27-52 ; ASCII values 65-27
const U_PRIORITY: u32 = 27;
const U_ASCII: u32 = 65;

// badge groups are elves in groups of 3
const GROUP_SIZE: usize = 3;

fn get_item_priority(item: char) -> u32 {
    let l_mod = L_ASCII - L_PRIORITY;
    let u_mod = U_ASCII - U_PRIORITY;
    let priority_mod: u32 = if item.is_lowercase() { l_mod } else { u_mod };

    return item as u32 - priority_mod;
}

fn find_duplicate_priority_sums(reader: BufReader<File>) -> u32 {
    let mut total = 0;
    for line_res in reader.lines() {
        let line = line_res.expect("Sack could not be read");

        // split the sacks
        let pivot = (line.len()) / 2;
        let sacks = line.split_at(pivot);
        assert!(sacks.0.len() == sacks.1.len());

        // find the matching item and add its priority to our total
        //  (assumes chars == bytes)
        for item in sacks.0.chars() {
            if sacks.1.contains(item) {
                total += get_item_priority(item);
                break;
            }
        }
    }
    return total;
}

fn find_badges_in_groups(reader: BufReader<File>) -> u32 {
    let mut total = 0;
    let lines: Vec<String> = reader.lines().map(|x| x.unwrap()).collect();

    for i in (0..lines.len()).step_by(GROUP_SIZE) {
        let cur_elf_group = lines.get(i..i+GROUP_SIZE).unwrap();

        // find the matching item and add its priority to our total
        let mut matches: HashSet<char> = HashSet::new();

        // find the items which match in groups 1 and 2
        for item in cur_elf_group[0].chars() {
            if cur_elf_group[1].contains(item) {
                matches.insert(item);
            }
        }
        // of those - find the single item which is in group 3
        for item in matches {
            if cur_elf_group[2].contains(item) {
                total += get_item_priority(item);
                break;
            }
        }
    }
    return total;
}

fn main() {
    println!("{}", find_duplicate_priority_sums(helpers::read_input()));
    println!("{}", find_badges_in_groups(helpers::read_input()));
}
