use anyhow::anyhow;
use std::ops::RangeInclusive;

type Result<T> = std::result::Result<T, anyhow::Error>;

fn parse_interval(s: &str) -> Result<RangeInclusive<i32>> {
    let (start, end) = s
        .split_once('-')
        .ok_or_else(|| anyhow!("invalid range {}", s))?;
    let start = start.parse::<i32>()?;
    let end = end.parse::<i32>()?;
    Ok(start..=end)
}

fn range_contains(a: &RangeInclusive<i32>, b: &RangeInclusive<i32>) -> bool {
    a.contains(b.start()) && a.contains(b.end())
}

fn range_overlaps(a: &RangeInclusive<i32>, b: &RangeInclusive<i32>) -> bool {
    a.contains(b.start()) || a.contains(b.end()) || range_contains(b, a)
}

fn main() -> Result<()> {
    let mut full_containment_count = 0;
    let mut overlapping_count = 0;

    for line in std::io::stdin().lines() {
        let line = line?;
        let line = line.trim();

        if line.is_empty() {
            continue;
        }

        let (first, second) = line
            .split_once(',')
            .ok_or_else(|| anyhow!("invalid range-pair: {}", line))?;

        let first = parse_interval(first)?;
        let second = parse_interval(second)?;

        let fully_contained = range_contains(&first, &second) || range_contains(&second, &first);

        if fully_contained {
            full_containment_count += 1;
        }

        if range_overlaps(&first, &second) {
            overlapping_count += 1;
        }
    }

    println!(
        "Number of pairs with intervals fully contained: {}",
        full_containment_count
    );
    println!(
        "Number of pairs with intervals that overlap: {}",
        overlapping_count
    );

    Ok(())
}
