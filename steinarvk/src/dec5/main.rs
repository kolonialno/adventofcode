use anyhow::{anyhow};
use itertools::Itertools;

type Result<T> = std::result::Result<T, anyhow::Error>;

fn parse_stacks(lines: &[String]) -> Result<Vec<Vec<char>>> {
    let n = lines
        .last()
        .ok_or_else(|| anyhow!("no setup lines"))?
        .split_whitespace()
        .count();

    let mut rv: Vec<Vec<char>> = (1..=n).map(|_| Vec::new()).collect();

    for line in &lines[..lines.len() - 1] {
        for (stack_index, chunk) in line.chars().chunks(4).into_iter().enumerate() {
            let chunk = chunk.collect::<String>();
            let chunk = chunk.trim();

            if chunk.is_empty() {
                continue;
            }

            let contents = chunk
                .strip_prefix('[')
                .ok_or_else(|| anyhow!("does not have expected format: {}", chunk))?
                .strip_suffix(']')
                .ok_or_else(|| anyhow!("does not have expected format: {}", chunk))?
                .chars()
                .next()
                .ok_or_else(|| anyhow!("does not have expected format: {}", chunk))?;

            rv[stack_index].push(contents);
        }
    }

    for stack in rv.iter_mut() {
        stack.reverse();
    }

    Ok(rv)
}

fn main() -> Result<()> {
    let mut setup_lines: Vec<String> = Vec::new();
    let mut stacks_9000: Vec<Vec<char>> = Vec::new();
    let mut stacks_9001: Vec<Vec<char>> = Vec::new();

    for line in std::io::stdin().lines() {
        let line = line?;

        if stacks_9000.is_empty() {
            if line.is_empty() {
                let parsed_stacks = parse_stacks(&setup_lines)?;
                stacks_9000.extend(parsed_stacks.clone());
                stacks_9001.extend(parsed_stacks.clone());
                continue;
            }

            setup_lines.push(line.clone());
        } else {
            if line.is_empty() {
                continue;
            }

            let items: Vec<&str> = line.split_whitespace().collect();

            if let [_move, number_of_crates, _from, origin_stack, _to, target_stack] = &items[..] {
                let number_of_crates = number_of_crates.parse::<usize>()?;
                let origin_stack = origin_stack.parse::<usize>()? - 1;
                let target_stack = target_stack.parse::<usize>()? - 1;

                let n = stacks_9001[origin_stack].len();
                let moved_9001: Vec<char> = stacks_9001[origin_stack][n - number_of_crates..].to_vec();

                stacks_9001[target_stack].extend(moved_9001);

                for _ in 0..number_of_crates {
                    let popped = stacks_9000[origin_stack]
                        .pop()
                        .ok_or_else(|| anyhow!("stack exhausted"))?;
                    stacks_9000[target_stack].push(popped);
                    stacks_9001[origin_stack].pop();
                }
            }
        }
    }

    for (i, stack) in stacks_9000.iter().enumerate() {
        eprintln!("9000: Stack {}: {:?}", i + 1, stack);
    }

    for (i, stack) in stacks_9001.iter().enumerate() {
        eprintln!("9001: Stack {}: {:?}", i + 1, stack);
    }

    println!(
        "9000: {}",
        stacks_9000
            .iter()
            .filter_map(|s| s.last())
            .collect::<String>()
    );

    println!(
        "9001: {}",
        stacks_9001
            .iter()
            .filter_map(|s| s.last())
            .collect::<String>()
    );

    Ok(())
}
