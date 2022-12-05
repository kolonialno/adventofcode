fn main() {
    let input = include_bytes!("input.txt");
    let mut lines = input.split(|e| *e as char == '\n').collect::<Vec<&[u8]>>();
    let index = lines.iter().position(|&r| r.is_empty()).unwrap();
    let num_stacks: usize = (lines[index - 1].iter().max().unwrap() - 48).into();

    let mut stacks_1: Vec<Vec<char>> = vec![Vec::new(); num_stacks];
    for l in &mut lines[..index - 1].iter().rev() {
        for (stack, item) in l.chunks(4).enumerate() {
            if item[1] as char != ' ' {
                stacks_1[stack].push((item[1] as char).clone())
            }
        }
    }
    let mut stacks_2 = stacks_1.clone();

    for l in &mut lines[index + 1..] {
        let command: Vec<usize> = l
            .split(|e| *e as char == ' ')
            .enumerate()
            .filter_map(|(pos, bytes)| {
                if [1, 3, 5].contains(&pos) {
                    return usize::from_str_radix(std::str::from_utf8(bytes).unwrap(), 10).ok();
                }
                None
            })
            .collect();

        for n in 0..command[0] {
            if let Some(item) = stacks_1[command[1] - 1].pop() {
                stacks_1[command[2] - 1].push(item);
            }
        }

        let mut new_items: Vec<char> = Vec::new();
        for n in 0..command[0] {
            if let Some(item) = stacks_2[command[1] - 1].pop() {
                new_items.push(item);
            }
        }
        new_items.reverse();
        stacks_2[command[2] - 1].append(&mut new_items);
    }
    let ans1: Vec<char> = stacks_1.into_iter().filter_map(|mut s| s.pop()).collect();
    dbg!(&ans1);
    let ans2: Vec<char> = stacks_2.into_iter().filter_map(|mut s| s.pop()).collect();
    dbg!(&ans2);

    println!("Hello, world!");
}
