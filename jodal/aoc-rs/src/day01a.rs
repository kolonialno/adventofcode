pub fn solve(input: &[i32]) -> Option<i32> {
    for i in input {
        for j in input {
            if i + j == 2020 {
                return Some(i * j);
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    #[test]
    fn example() {
        let input = vec![1721, 979, 366, 299, 675, 1456];
        let result = super::solve(&input);
        assert_eq!(result.unwrap(), 514_579);
    }
}
