type Result<T> = std::result::Result<T, anyhow::Error>;

fn to_snafu(mut x: i64) -> Result<String> {
    let mut xs: Vec<char> = Vec::new();
    while x != 0 {
        assert!(x > 0);
        let (d, v) = match x % 5 {
            1 => ('1', 1),
            2 => ('2', 2),
            3 => ('=', -2),
            4 => ('-', -1),
            _ => ('0', 0),
        };
        xs.push(d);
        x -= v;
        assert!(x % 5 == 0);
        x /= 5;
    }
    Ok(xs.iter().rev().collect())
}

fn from_snafu(s: &str) -> Result<i64> {
    let mut m = 1;
    let mut rv = 0;
    for x in s.chars().rev() {
        rv += m * match x {
            '2' => 2,
            '1' => 1,
            '0' => 0,
            '-' => -1,
            '=' => -2,
            ch => anyhow::bail!("bad char {}", ch),
        };
        m *= 5;
    }
    Ok(rv)
}

fn main() -> Result<()> {
    let mut running_total = 0;

    for line in std::io::stdin().lines() {
        running_total += from_snafu(line?.trim())?;
    }

    println!("Answer: {}", to_snafu(running_total)?);

    Ok(())
}
