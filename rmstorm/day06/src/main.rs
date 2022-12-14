use itertools::Itertools;

fn main() {
    let input = include_bytes!("input.txt");
    let res: usize = input
        .windows(14)
        .enumerate()
        .find_map(|(pos, e)| {
            if e.into_iter().all_unique() {
                return Some(pos);
            }
            None
        })
        .unwrap()
        + 14;
    dbg!(res);
}
