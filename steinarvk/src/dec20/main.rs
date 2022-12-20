type Result<T> = std::result::Result<T, anyhow::Error>;

fn mix(mixable: &mut Vec<(usize, i64)>, initial_numbers: &[i64]) {
    let n = initial_numbers.len() as i64;
    for (i, value) in initial_numbers.iter().enumerate() {
        let pos = mixable.iter().position(|x| *x == (i, *value)).unwrap();
        let new_pos = (pos as i64 + *value) % (n - 1);
        let new_pos = (new_pos + (n - 1)) % (n - 1);
        let item = mixable.remove(pos);
        mixable.insert(new_pos as usize, item);
    }
}

fn score(numbers: &[(usize, i64)]) -> i64 {
    let n = numbers.len();
    let zero_pos = numbers.iter().position(|x| x.1 == 0).unwrap();
    let a = numbers[(zero_pos + 1000) % n].1;
    let b = numbers[(zero_pos + 2000) % n].1;
    let c = numbers[(zero_pos + 3000) % n].1;
    a + b + c
}

fn main() -> Result<()> {
    let list = std::io::stdin()
        .lines()
        .map(|x| x.unwrap().trim().parse::<i64>())
        .collect::<std::result::Result<Vec<i64>, _>>()?;

    let mut one: Vec<(usize, i64)> = list.iter().enumerate().map(|(i, x)| (i, *x)).collect();
    mix(&mut one, &list);
    println!("Answer part A: {}", score(&one));

    let list: Vec<i64> = list.iter().map(|x| x * 811589153).collect();
    let mut two: Vec<(usize, i64)> = list.iter().enumerate().map(|(i, x)| (i, *x)).collect();
    for _ in 0..10 {
        mix(&mut two, &list);
    }
    println!("Answer part B: {}", score(&two));

    Ok(())
}
