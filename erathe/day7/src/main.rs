use itertools::Itertools;
use std::{collections::HashMap, str::FromStr, time::Instant};

fn main() {
    let input = include_str!("../input.txt");

    let t1 = Instant::now();
    println!("part 1: {:?} in {:?}", part1(input), t1.elapsed());
}

fn part1(input: &str) -> Option<usize> {
    Some(
        input
            .lines()
            .map(|l| l.parse::<Hand>().unwrap())
            .sorted()
            .enumerate()
            .map(|(idx, hand)| (idx + 1) * hand.bid)
            .sum(),
    )
}

#[derive(Debug)]
struct Hand {
    cards: Vec<usize>,
    bid: usize,
    strength: usize,
}

impl FromStr for Hand {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut strength = 1;
        let (hand, bid) = s.split_once(" ").unwrap();
        let mut frq_map = hand
            .chars()
            .fold(HashMap::new(), |mut map, val| {
                // part 2
                if val == 'J' {
                    return map;
                }
                map.entry(val).and_modify(|frq| *frq += 1).or_insert(1);
                map
            })
            .into_values()
            .collect_vec();

        // part 2 start
        if frq_map.len() == 0 {
            frq_map = Vec::from([0]);
        }
        let js = hand.chars().filter(|c| *c == 'J').count();
        let max = frq_map.iter().max().unwrap();
        let (max_idx, _) = frq_map.iter().copied().find_position(|v| v == max).unwrap();
        if let Some(entry) = frq_map.get_mut(max_idx) {
            *entry += js;
        }
        // part 2 end
        match frq_map[..] {
            [_] => strength = 7,
            [a, b] => {
                if [a, b].into_iter().any(|v| v == 4) {
                    strength = 6;
                } else {
                    strength = 5;
                }
            }
            [a, b, c] => {
                if [a, b, c].into_iter().any(|v| v == 3) {
                    strength = 4;
                } else {
                    strength = 3;
                }
            }
            [_, _, _, _] => strength = 2,
            [_, _, _, _, _] => strength = 1,
            _ => panic!("should not happen"),
        };
        let cards = hand
            .chars()
            .map(|c| match c {
                'A' => 14,
                'K' => 13,
                'Q' => 12,
                'J' => 1,
                'T' => 10,
                // part 1
                // 'A' => 14,
                // 'K' => 13,
                // 'Q' => 12,
                // 'J' => 11,
                // 'T' => 10,
                _ => c.to_string().parse::<usize>().unwrap(),
            })
            .collect_vec();

        Ok(Hand {
            cards,
            bid: bid.parse::<usize>().unwrap(),
            strength,
        })
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.strength != other.strength {
            return Some(self.strength.cmp(&other.strength));
        } else {
            for i in 0..self.cards.len() {
                match self.cards[i].cmp(&other.cards[i]) {
                    std::cmp::Ordering::Less => return Some(std::cmp::Ordering::Less),
                    std::cmp::Ordering::Equal => continue,
                    std::cmp::Ordering::Greater => return Some(std::cmp::Ordering::Greater),
                }
            }
        }
        Some(std::cmp::Ordering::Equal)
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.strength.cmp(&other.strength) == std::cmp::Ordering::Equal && self.cards == other.cards
    }
}

impl Eq for Hand {}
