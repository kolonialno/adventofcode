use std::fs::File;
use std::io::{self, BufRead};

fn solve() {
    let file = File::open("./data/0.txt");
    let reader = io::BufReader::new(file.unwrap());

    for line in reader.lines() {
        let line_string = line.unwrap();
        println!("{}", line_string);
    }
}

pub fn __main__() {
    solve();
}