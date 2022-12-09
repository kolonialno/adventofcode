type Result<T> = std::result::Result<T, anyhow::Error>;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Visibility {
    Hidden,
    Visible,
}

fn visible_anywhere(a: Visibility, b: Visibility) -> Visibility {
    match (a, b) {
        (Visibility::Hidden, Visibility::Hidden) => Visibility::Hidden,
        _ => Visibility::Visible,
    }
}

fn directional_visibility<T>(tree_heights: T) -> Vec<Visibility>
where
    T: Iterator<Item = u8>,
{
    tree_heights
        .scan(0_u8, |obstacle, target| {
            Some(if *obstacle < target {
                *obstacle = target;
                Visibility::Visible
            } else {
                Visibility::Hidden
            })
        })
        .collect()
}

fn ray_scenic_score<T>(house_height: u8, tree_heights: T) -> i64
where
    T: Iterator<Item = u8>,
{
    tree_heights
        .scan(false, |stop, tree_height| {
            if *stop {
                None
            } else {
                if tree_height >= house_height {
                    *stop = true;
                }
                Some(())
            }
        })
        .count()
        .try_into()
        .unwrap()
}

fn bidirectional_visibility(trees: &[u8]) -> Vec<Visibility> {
    let forward: Vec<Visibility> = directional_visibility(trees.iter().cloned());
    directional_visibility(trees.iter().cloned().rev())
        .iter()
        .cloned()
        .rev()
        .zip(forward.iter())
        .map(|(a, b)| visible_anywhere(a, *b))
        .collect()
}

fn main() -> Result<()> {
    let mut columns: Vec<Vec<u8>> = Vec::new();
    let mut rows: Vec<Vec<u8>> = Vec::new();

    for (i, line) in std::io::stdin().lines().enumerate() {
        let line = line?;
        let line = line.trim();
        let trees = line.as_bytes();

        rows.push(Vec::from(trees));

        if i == 0 {
            columns = trees.iter().map(|x| vec![*x]).collect();
            continue;
        }

        columns
            .iter_mut()
            .zip(trees.iter())
            .for_each(|(v, x)| v.push(*x));
    }

    let horizontal_visibility: Vec<Vec<Visibility>> = rows
        .iter()
        .map(|x| bidirectional_visibility(&x[..]))
        .collect();
    let vertical_visibility: Vec<Vec<Visibility>> = columns
        .iter()
        .map(|x| bidirectional_visibility(&x[..]))
        .collect();

    let mut count: i32 = 0;
    let mut highest_scenic_score: i64 = 0;

    for (i, row) in rows.iter().enumerate() {
        for (j, value) in row.iter().enumerate() {
            let v = visible_anywhere(horizontal_visibility[i][j], vertical_visibility[j][i]);
            if v == Visibility::Visible {
                count += 1;
            }
            print!("{}", if v == Visibility::Visible { "_" } else { "X" });

            let col = &columns[j];

            let ray_scores = vec![
                ray_scenic_score(*value, col[..i].iter().rev().cloned()),
                ray_scenic_score(*value, row[..j].iter().rev().cloned()),
                ray_scenic_score(*value, col[(i + 1)..].iter().cloned()),
                ray_scenic_score(*value, row[(j + 1)..].iter().cloned()),
            ];

            let scenic_score = ray_scores.iter().product::<i64>();

            highest_scenic_score = highest_scenic_score.max(scenic_score);
        }
        println!();
    }

    println!("Total number of visible trees: {}", count);
    println!("Highest scenic score: {}", highest_scenic_score);

    Ok(())
}
