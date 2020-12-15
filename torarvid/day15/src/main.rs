fn main() {
    println!("Part1: {}", solve("2,20,0,4,1,17", 2020));
    println!("Part2: {}", solve("2,20,0,4,1,17", 30_000_000));
}

fn solve(input: &str, limit: usize) -> usize {
    let starting_numbers = input
        .split(",")
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    let mut number_vec: Vec<Option<(usize, Option<usize>)>> = Vec::new();
    let mut i = 0;
    let mut previous_number = *starting_numbers.last().unwrap();
    while i < limit {
        let (new_number, round_seen_last) = if i < starting_numbers.len() {
            (starting_numbers[i], None)
        } else {
            let (round_seen_last, round_seen_before_that) =
                number_vec.get(previous_number).unwrap().unwrap();
            if let Some(round_seen_before_that_v) = round_seen_before_that {
                let new_number = round_seen_last - round_seen_before_that_v;
                let seen_last = number_vec.get(new_number).and_then(|e| e.map(|ee| ee.0));
                (new_number, seen_last)
            } else {
                let new_number = 0;
                let zero_seen_last = number_vec.get(new_number).and_then(|e| e.map(|ee| ee.0));
                (new_number, zero_seen_last)
            }
        };
        if new_number >= number_vec.len() {
            number_vec.reserve((new_number - number_vec.len() + 1) * 2);
            while number_vec.len() <= new_number {
                number_vec.push(None);
            }
        }
        number_vec[new_number] = Some((i + 1, round_seen_last));
        previous_number = new_number;
        i += 1;
    }
    previous_number
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        assert_eq!(solve("0,3,6", 2020), 436);
    }
}
