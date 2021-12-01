use std::fs::File;
use std::io::{BufRead, BufReader, Error, ErrorKind};

fn print_type_of<T>(_: &T) {
    println!("type of input: {}", std::any::type_name::<T>())
}

// function takes path in the form of string slice and returns enum
// which contains vector of integers on success or IO error type, see `std::io::Error`
fn read_input(path: &str) -> Result<Vec<i64>, Error> {
    let file = File::open(path)?; // open file by given path
    // wrap file into generic buffered reader, it will use 4 KB buffer internally
    // to reduce number of syscalls, thus improving performance
    let br = BufReader::new(file);
    // create an empty vector, type of the stored elements will be inferred
    let mut v = Vec::new();
    // br.lines() creates an iterator over lines in the reader
    // see: https://doc.rust-lang.org/std/io/trait.BufRead.html#method.lines
    for line in br.lines() {
        // IO operations generally can return error, we check if got
        // an error,in which case we return this error as the function result
        let line = line?;
        let n = line   
            .trim() // trim "whitespaces"
            .parse() // call `str::parse::<i64>(&self)` method on the trimmed line, which parses integer
            .map_err(|e| Error::new(ErrorKind::InvalidData, e))?; // parse() can return error (e.g. for string "abc"), here if we got it, we convert it to `std::io::Error` type and return it as function result
        v.push(n); // push acquired integer to the vector
    }
    Ok(v) // everything is Ok, return vector
}

fn main() -> Result<(), Error>{
    // Get input
    let depths = read_input("input.txt")?;
    print_type_of(&depths);

    // Task 1
    let mut index = 1;
    let mut increasing_depths = 0;

    while index < depths.len() {
        if depths[index - 1] < depths[index]{
            increasing_depths = increasing_depths + 1;
        }
        index = index + 1;
    }
    println!("Number of increasing depths is: {}", increasing_depths);

    // Task 2
    index = 0;
    increasing_depths = 0;
    let mut window_a: i64;
    let mut window_b: i64 = depths[0] + depths[1] + depths[2];

    while index < depths.len() - 3 {
        window_a = window_b;
        window_b = depths[index+1] + depths[index+2] + depths[index+3];
        if window_a < window_b{
            increasing_depths = increasing_depths + 1;
        }
        index = index + 1;
    }
    println!("Number of increasing windowed depths is: {}", increasing_depths);

    Ok(())
}
