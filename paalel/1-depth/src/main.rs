use std::fs::File;
use std::io::{BufRead, BufReader, Error, ErrorKind};

fn read_input(path: &str) -> Result<Vec<i64>, Error> {
    let file = File::open(path)?;
    let br = BufReader::new(file);
    let mut v = Vec::new();

    for line in br.lines() {
        let line = line?;
        let n = line
            .trim()
            .parse()
            .map_err(|e| Error::new(ErrorKind::InvalidData, e))?;

        v.push(n); // push acquired integer to the vector
    }
    Ok(v)
}

fn main() -> Result<(), Error> {
    // Get input
    let depths = read_input("input.txt")?;

    // Problem 1
    // Create windows [A, B] and check A < B
    let mut increasing_depths = depths.windows(2).filter(|w| w[0] < w[1]).count();
    println!("Number of increasing depths is: {}", increasing_depths);

    // Problem 2
    // (A + B + C) - (B + C + D) = D - A so we only need to check if the last in the window is
    // larger then the first.
    // Create windows [A, B, C, D] and check A < D
    increasing_depths = depths.windows(4).filter(|w| w[0] < w[3]).count();
    println!(
        "Number of increasing windowed depths is: {}",
        increasing_depths
    );

    Ok(())
}
