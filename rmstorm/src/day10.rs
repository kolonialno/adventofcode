use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

fn read<R: Read>(io: R) -> Result<Vec<Vec<char>>, Error> {
    Ok(BufReader::new(io)
        .lines()
        .map(|line| line.unwrap().trim().chars().collect())
        .collect())
}

pub fn day10() {
    let pairs = HashMap::from([('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]);
    let closer_penalties = HashMap::from([(')', 3), (']', 57), ('}', 1197), ('>', 25137)]);
    let closer_scores = HashMap::from([('(', 1), ('[', 2), ('{', 3), ('<', 4)]);
    let sets = read(File::open("inputs/day10.txt").unwrap()).unwrap();

    let mut answer1 = 0;
    let mut answer2: Vec<u64> = Vec::new();
    for s in sets {
        let mut sol = s.clone();
        let mut prev_len = 0;

        while sol.len() != prev_len {
            prev_len = sol.len();

            let mut scheduled_for_murder = sol.len() + 1;
            for (i, e) in sol.iter().enumerate() {
                if i + 1 < sol.len() && !closer_penalties.contains_key(e) && pairs[e] == sol[i + 1]
                {
                    scheduled_for_murder = i;
                    break;
                }
            }
            if scheduled_for_murder != sol.len() + 1 {
                sol.drain(scheduled_for_murder..scheduled_for_murder + 2);
            }
        }
        if sol.iter().any(|e| closer_penalties.contains_key(e)) {
            if let Some(closer) = sol.iter().find(|e| closer_penalties.contains_key(e)) {
                answer1 += closer_penalties[closer];
            }
        } else {
            answer2.push(
                sol.iter()
                    .rev()
                    .fold(0, |acc, x| (acc * 5) + closer_scores[x]),
            );
        }
    }
    answer2.sort();
    println!("answer1: {:?}", answer1);
    println!("answer2: {:?}", answer2[(answer2.len() - 1) / 2]);
}
