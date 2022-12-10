type Result<T> = std::result::Result<T, anyhow::Error>;

fn main() -> Result<()> {
    let mut to_observe: std::collections::VecDeque<i64> = (20..=220).step_by(40).collect();
    let mut pixel_row: Vec<char> = Vec::new();
    let mut screen: Vec<String> = Vec::new();

    // expect: 1 1 1 4 4 -1
    let (final_cycles, final_x, final_score) = std::io::stdin()
        .lines()
        .filter_map(|line| {
            let line = line.unwrap();
            let tokens: Vec<&str> = line.trim().split(' ').collect();
            match &tokens[..] {
                [] => None,
                ["noop"] => Some((1, 0)),
                ["addx", x] => Some((2, x.parse::<i64>().unwrap())),
                _ => panic!("unexpected input: {}", line),
            }
        })
        .fold((0_i64, 1_i64, 0), |(c0, x, s), (dc, dx)| {
            let c1 = c0 + dc;
            let mut score = 0;

            while let Some(co) = to_observe.front() {
                let co = *co;

                if co > c1 {
                    break;
                }
                assert!(co >= c0);
                to_observe.pop_front();

                let observation_score = co * x;
                println!(
                    "Value during cycle {}: {} (score {})",
                    co, x, observation_score
                );
                score += observation_score;
            }

            for c in c0..c1 {
                let pixel_being_drawn_x = c % 40;
                if pixel_being_drawn_x == 0 && !pixel_row.is_empty() {
                    assert!(pixel_row.len() == 40);
                    screen.push(pixel_row.iter().collect());
                    pixel_row = Vec::new();
                    //println!("Pixels: {}", pixel_row.join(""));
                    //pixel_row.clear();
                }

                let sprite_position = x;
                let lit = (pixel_being_drawn_x - sprite_position).abs() <= 1;
                pixel_row.push(if lit { '#' } else { '.' });
            }

            (c1, x + dx, s + score)
        });

    assert!(pixel_row.len() == 40);
    screen.push(pixel_row.into_iter().collect());

    println!();
    println!("Number of cycles executed: {}", final_cycles);
    println!("Final value of x: {}", final_x);
    println!("Total score (i.e. answer question A): {}", final_score);
    println!();
    println!("{}", screen.join("\n"));

    Ok(())
}
