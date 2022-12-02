use std::fs::File;
use std::io::BufReader;

pub(crate) fn read_input() -> BufReader<File> {
    let input_file =
        File::open("./data/input.txt").expect("Should have been able to read the input file");
    return BufReader::new(input_file);
}