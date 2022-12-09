type Result<T> = std::result::Result<T, anyhow::Error>;

fn main() -> Result<()> {
    let mut running_total = 0;

    let mut totals: Vec<i32> = Vec::new();

    for line in std::io::stdin().lines() {
        let line = line?;
        if line.trim().is_empty() {
            totals.push(running_total);
            running_total = 0;
            continue;
        }

        running_total += line.parse::<i32>()?;
    }

    totals.push(running_total);

    totals.sort();

    println!("Max 1: {}", totals.last().unwrap());
    println!(
        "Max 3: {}",
        totals[totals.len() - 3..].iter().sum::<i32>()
    );

    Ok(())
}
