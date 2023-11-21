use std::collections::HashSet;
use std::io::Read;

type Result<T> = std::result::Result<T, anyhow::Error>;

fn find_marker(s: &str, n: usize) -> Option<usize> {
    s.as_bytes()
        .windows(n)
        .enumerate().find(|(_, s)| HashSet::<&u8>::from_iter(s.iter()).len() == n)
        .map(|(i, _)| i + n)
}

fn main() -> Result<()> {
    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s)?;

    println!(
        "Start marker: {}",
        find_marker(&s, 4).expect("expected marker")
    );
    println!(
        "End marker: {}",
        find_marker(&s, 14).expect("expected marker")
    );

    Ok(())
}
