pub fn run() {
    let (min_x, max_x, min_y, max_y) = (96, 125, -144, -98);

    let mut high_y = 0;
    for vx in 0..=20 {
        for vy in 0..=500 {
            let y = sim(min_x, max_x, min_y, max_y, vx, vy);
            if y > high_y {
                high_y = y;
            }
        }
    }
    println!("Part 1: {}", high_y);

    let mut count = 0;
    for vx in 0..=150 {
        for vy in -2000..=2000 {
            if sim2(min_x, max_x, min_y, max_y, vx, vy) {
                count += 1;
            }
        }
    }

    println!("Part 2: {}", count);
}

fn sim(min_x: i32, max_x: i32, min_y: i32, max_y: i32, mut vx: i32, mut vy: i32) -> i32 {
    let mut x = 0;
    let mut y = 0;
    let mut high_y = 0;
    while x <= max_x && y >= min_y {
        if x >= min_x && y <= max_y {
            return high_y;
        }
        x += vx;
        y += vy;
        if y > high_y {
            high_y = y;
        }
        vx += if vx > 0 { -1 } else if vx < 0 { 1 } else { 0 };
        vy -= 1;
    }
    -1
}

fn sim2(min_x: i32, max_x: i32, min_y: i32, max_y: i32, mut vx: i32, mut vy: i32) -> bool {
    let mut x = 0;
    let mut y = 0;
    while x <= max_x && y >= min_y {
        if x >= min_x && y <= max_y {
            return true;
        }
        x += vx;
        y += vy;
        vx += if vx > 0 { -1 } else if vx < 0 { 1 } else { 0 };
        vy -= 1;
    }
    false
}
