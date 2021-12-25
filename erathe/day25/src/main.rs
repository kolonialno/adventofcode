use itertools::Itertools;
fn move_snail(v: &Vec<Vec<char>>, m: char) -> (bool, Vec<Vec<char>>) {
    let mut n_vec = v.clone();
    let mut moved = false;
    let rows = v.len();
    let cols = v[0].len();

    for i in 0..rows {
        for j in 0..cols {
            if v[i][j] == m {
                if m == '>' {
                    if v[i][(j + 1) % cols] == '.' {
                        n_vec[i][(j + 1) % cols] = m;
                        n_vec[i][j] = '.';
                        moved = true;
                    }
                } else {
                    if v[(i + 1) % rows][j] == '.' {
                        n_vec[(i + 1) % rows][j] = m;
                        n_vec[i][j] = '.';
                        moved = true;
                    }
                }
            }
        }
    }
    (moved, n_vec)
}

fn main() {
    let mut inp = include_str!("../input.txt")
        .lines()
        .map(|l| l.chars().collect_vec())
        .collect_vec();

    let mut i = 0;
    loop {
        i += 1;
        let (h, h_vec) = move_snail(&inp, '>');
        let (v, v_vec) = move_snail(&h_vec, 'v');
        if !h && !v {
            break;
        }
        inp = v_vec;
    }
    println!("{}", i);
}
