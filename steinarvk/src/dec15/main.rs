use regex::Regex;
use std::ops::RangeInclusive;

type Result<T> = std::result::Result<T, anyhow::Error>;

fn merge_into_sorted_disjoint_set(
    sorted_disjoint_intervals: Vec<RangeInclusive<i64>>,
    addition: RangeInclusive<i64>,
) -> Vec<RangeInclusive<i64>> {
    let mut xs = sorted_disjoint_intervals;
    xs.push(addition);
    xs.sort_by(|a, b| a.start().cmp(b.start()));

    let mut i = 0;
    while (i + 1) < xs.len() {
        let left = &xs[i];
        let right = &xs[i + 1];
        if left.contains(right.start()) {
            // Merge two adjacent overlapping intervals
            let start = *left.start();
            let end = *left.end().max(right.end());
            xs[i] = start..=end;
            xs.remove(i + 1);
        } else {
            i += 1;
        }
    }

    xs
}

fn blocked_positions_on_line(
    lines: &[((i64, i64), (i64, i64))],
    line_y: i64,
    exclude_current_beacon: bool,
) -> Vec<RangeInclusive<i64>> {
    lines
        .iter()
        .filter_map(|((sx, sy), (bx, by))| {
            let r = (sx - bx).abs() + (sy - by).abs();
            let delta_y = (line_y - sy).abs();
            let radius_at_line = r - delta_y;

            if radius_at_line < 0 {
                None
            } else {
                let x0 = sx - radius_at_line;
                let x1 = sx + radius_at_line;
                if *by != line_y || !exclude_current_beacon {
                    Some(x0..=x1)
                } else if bx < sx {
                    Some((x0 + 1)..=x1)
                } else if bx > sx {
                    Some(x0..=(x1 - 1))
                } else {
                    None
                }
            }
        })
        .fold(Vec::new(), merge_into_sorted_disjoint_set)
}

fn union_length(xs: &[RangeInclusive<i64>]) -> i64 {
    xs.iter().map(|x| x.end() - x.start() + 1).sum()
}

fn line_is_wholly_blocked(xs: &[RangeInclusive<i64>], sz: i64) -> bool {
    xs.iter().any(|x| x.contains(&0) && x.contains(&sz))
}

fn gaps_between(xs: &[RangeInclusive<i64>], start: i64, end: i64) -> Vec<RangeInclusive<i64>> {
    let mut rv = Vec::new();
    let valid = start..=end;
    let first_start = *xs.first().unwrap().start();
    let last_end = *xs.last().unwrap().end();

    if first_start > start {
        rv.push(start..=(first_start - 1));
    }

    for (a, b) in xs.iter().zip(&xs[1..]) {
        if a.end() + 1 >= *b.start() {
            continue;
        }
        let new_start = *a.end() + 1;
        let new_end = *b.start() - 1;

        assert!(new_start <= new_end);

        if valid.contains(&new_start)
            || valid.contains(&new_end)
            || (new_start..=new_end).contains(&start)
        {
            rv.push(new_start..=new_end);
        }
    }

    if end < last_end {
        rv.push((last_end + 1)..=end);
    }

    rv
}

fn tuning_frequency(x: i64, y: i64) -> i64 {
    x * 4_000_000 + y
}

fn main() -> Result<()> {
    let re = Regex::new(r"Sensor at x=(?P<sensor_x>[0-9-]+), y=(?P<sensor_y>[0-9-]+): closest beacon is at x=(?P<beacon_x>[0-9-]+), y=(?P<beacon_y>[0-9-]+)").unwrap();

    let parsed_lines: Vec<((i64, i64), (i64, i64))> = std::io::stdin()
        .lines()
        .map(|line| {
            let line = line.unwrap();
            let captures = re.captures(line.trim()).unwrap();
            let sx: i64 = captures["sensor_x"].parse().unwrap();
            let sy: i64 = captures["sensor_y"].parse().unwrap();
            let bx: i64 = captures["beacon_x"].parse().unwrap();
            let by: i64 = captures["beacon_y"].parse().unwrap();
            ((sx, sy), (bx, by))
        })
        .collect();

    let is_small_testcase = parsed_lines.iter().map(|v| v.0 .0).max().unwrap() <= 20;

    let test_line_y = if is_small_testcase { 10 } else { 2_000_000 };
    let area_size = if is_small_testcase { 20 } else { 4_000_000 };

    // Part one
    let sets = blocked_positions_on_line(&parsed_lines, test_line_y, true);
    println!(
        "Blocked positions on line y={}: {}",
        test_line_y,
        union_length(&sets)
    );

    // Part two, kind of brute-force
    for y in 0..=area_size {
        let sets = blocked_positions_on_line(&parsed_lines, y, false);
        if !line_is_wholly_blocked(&sets, area_size) {
            for gap in gaps_between(&sets, 0, area_size) {
                for x in gap {
                    let answer = tuning_frequency(x, y);
                    println!("with area size={}: {},{}: {}", area_size, x, y, answer);
                }
            }
        }
    }

    Ok(())
}
