use std::collections::HashSet;
use std::collections::VecDeque;

type Result<T> = std::result::Result<T, anyhow::Error>;

type Point3 = (i32, i32, i32);

fn neighbours(p: Point3) -> [Point3; 6] {
    let (x, y, z) = p;
    [
        (x + 1, y, z),
        (x - 1, y, z),
        (x, y + 1, z),
        (x, y - 1, z),
        (x, y, z + 1),
        (x, y, z - 1),
    ]
}

fn main() -> Result<()> {
    let mut points: HashSet<Point3> = HashSet::new();

    for line in std::io::stdin().lines() {
        let line = line?;
        let p: Vec<i32> = line
            .trim()
            .split(',')
            .map(|x| x.parse::<i32>().unwrap())
            .collect();
        assert!(p.len() == 3);
        let p = (p[0], p[1], p[2]);
        points.insert(p);
    }

    println!(
        "Answer: {}",
        points
            .iter()
            .map(|x| neighbours(*x)
                .iter()
                .filter(|n| !points.contains(n))
                .count())
            .sum::<usize>(),
    );

    let xs: Vec<i32> = points.iter().map(|x| x.0).collect();
    let ys: Vec<i32> = points.iter().map(|x| x.1).collect();
    let zs: Vec<i32> = points.iter().map(|x| x.2).collect();

    let x0 = *xs.iter().min().unwrap() - 1;
    let x1 = *xs.iter().max().unwrap() + 1;
    let y0 = *ys.iter().min().unwrap() - 1;
    let y1 = *ys.iter().max().unwrap() + 1;
    let z0 = *zs.iter().min().unwrap() - 1;
    let z1 = *zs.iter().max().unwrap() + 1;

    let mut outside_points: HashSet<Point3> = HashSet::new();

    let mut q: VecDeque<Point3> = VecDeque::new();
    q.push_back((x0, y0, z0));

    while let Some(p) = q.pop_front() {
        outside_points.insert(p);
        for n in neighbours(p) {
            let (x, y, z) = n;
            if points.contains(&n) || outside_points.contains(&n) {
                continue;
            }
            if x < x0 || y < y0 || z < z0 || x > x1 || y > y1 || z > z1 {
                continue;
            }
            outside_points.insert(n);
            q.push_back(n);
        }
    }

    println!(
        "Answer: {}",
        points
            .iter()
            .map(|x| neighbours(*x)
                .iter()
                .filter(|n| outside_points.contains(n))
                .count())
            .sum::<usize>(),
    );

    Ok(())
}
