fn main() {
    let input_string = include_str!("../input.txt");

    // Task modifier
    let option = 2;
    let answer = match option {
        1 => get_all_rucksack_priorities(input_string),
        2 => get_all_elf_squad_badges(input_string),
        _ => 0,
    };

    println!("{}", answer);
}

fn get_all_rucksack_priorities(rucksacks: &str) -> u16 {
    let mut total_priorities: u16 = 0;
    for rucksack in rucksacks.lines() {
        total_priorities += get_one_rucksack_priority(rucksack);
    }
    return total_priorities;
}

fn get_one_rucksack_priority(rucksack: &str) -> u16 {
    let (comp1, comp2) = get_one_rucksack_compartments(rucksack);
    return get_shared_items_in_rucksack(comp1, comp2);
}

fn get_all_elf_squad_badges(rucksacks: &str) -> u16 {
    // Denotes when to start counting again
    let mut num_rucksacks = 0;
    for _ in rucksacks.lines() {
        num_rucksacks += 1;
    }
    println!("{:?}", num_rucksacks);
    assert_eq!(num_rucksacks % 3, 0);

    let rucksack_vec: Vec<&str> = rucksacks.split("\n").filter(|&v| !v.is_empty()).collect();
    let mut sum_priorities = 0;
    for i in 0..num_rucksacks / 3 {
        let rucksack1 = rucksack_vec[3 * i];
        let rucksack2 = rucksack_vec[3 * i + 1];
        let rucksack3 = rucksack_vec[3 * i + 2];
        sum_priorities += get_elf_squad_badge(rucksack1, rucksack2, rucksack3);
    }
    return sum_priorities;
}

fn get_elf_squad_badge(rucksack1: &str, rucksack2: &str, rucksack3: &str) -> u16 {
    for item1 in rucksack1.chars() {
        for item2 in rucksack2.chars() {
            for item3 in rucksack3.chars() {
                if item1 == item2 && item2 == item3 {
                    // Found the item, no other items can exist
                    return map_char_item_to_int(item1);
                }
            }
        }
    }
    panic!("The squad doesn't have a common badge")
}

fn get_one_rucksack_compartments(rucksack: &str) -> (&str, &str) {
    let num_items = rucksack.len();
    assert_eq!(num_items % 2, 0);

    let comp1 = &rucksack[..num_items / 2];
    let comp2 = &rucksack[num_items / 2..];
    return (comp1, comp2);
}

fn get_shared_items_in_rucksack(comp1: &str, comp2: &str) -> u16 {
    assert_eq!(comp1.len(), comp2.len());
    for item1 in comp1.chars() {
        for item2 in comp2.chars() {
            if item1 == item2 {
                // Found the item, no other items can exist
                return map_char_item_to_int(item1);
            }
        }
    }
    panic!("Didn't find any items present in both compartments")
}

fn map_char_item_to_int(item: char) -> u16 {
    assert!(item.is_alphabetic());
    if item.is_uppercase() {
        return item as u16 - 65 + 27;
    } else {
        return item as u16 - 97 + 1;
    }
}
