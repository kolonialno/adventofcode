use itertools::Itertools;

fn convert_ascii_u8_to_value(byte: &u8) -> i32 {
    if byte < &('a' as u8) {
        (byte + 27 - 'A' as u8).into()
    } else {
        (byte + 1 - 'a' as u8).into()
    }
}

fn main() {
    let input = include_bytes!("input.txt");
    let backpack_contents = input.split(|e| *e as char == '\n').collect::<Vec<&[u8]>>();
    let matched_items = backpack_contents
        .iter()
        .map(|backpack| {
            let midpoint = backpack.len() / 2;
            for item in backpack[..midpoint].iter() {
                if backpack[midpoint..].contains(item) {
                    return item;
                }
            }
            panic!("No matched item found in second compartment");
        })
        .collect::<Vec<&u8>>();

    dbg!(matched_items
        .iter()
        .map(|e| convert_ascii_u8_to_value(*e))
        .sum::<i32>());

    let badges = backpack_contents
        .into_iter()
        .chunks(3)
        .into_iter()
        .map(|e| {
            let backpacks: Vec<&[u8]> = e.collect();
            for item in backpacks[0].iter() {
                if backpacks[1].contains(item) {
                    if backpacks[2].contains(item) {
                        return item;
                    }
                }
            }
            panic!("No badge found");
        })
        .collect::<Vec<&u8>>();

    dbg!(badges
        .iter()
        .map(|e| convert_ascii_u8_to_value(*e))
        .sum::<i32>());
}
