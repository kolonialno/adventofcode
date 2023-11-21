fn main() {
    let input = include_bytes!("input.txt");
    let lines = input.split(|e| *e as char == '\n').collect::<Vec<&[u8]>>();
    let index = lines.iter().position(|&r| r.is_empty()).unwrap();
    let num_stacks: usize = (lines[index - 1].iter().max().unwrap() - 48).into();

    let mut stacks: Vec<Vec<char>> = vec![Vec::new(); num_stacks];
    for l in &mut lines[..index - 1].iter().rev() {
        for (stack, item) in l.chunks(4).enumerate() {
            if item[1] as char != ' ' {
                stacks[stack].push((item[1] as char).clone())
            }
        }
    }
    execute_instructions(&lines[index + 1..], stacks.clone(), true);
    execute_instructions(&lines[index + 1..], stacks.clone(), false);
}

fn execute_instructions(lines: &[&[u8]], mut stacks: Vec<Vec<char>>, reverse: bool) {
    for l in lines.iter() {
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

        let mut new_items: Vec<char> = (0..command[0])
            .filter_map(|_| stacks[command[1] - 1].pop())
            .collect();
        if reverse {
            new_items.reverse();
        }
        stacks[command[2] - 1].append(&mut new_items);
    }
    dbg!(stacks
        .into_iter()
        .filter_map(|mut s| s.pop())
        .collect::<String>());
}
