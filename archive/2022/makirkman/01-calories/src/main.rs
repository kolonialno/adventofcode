use std::fs::File;
use std::io::{BufRead, BufReader};

fn read_input() -> BufReader<File> {
    let input_file =
        File::open("./data/input.txt").expect("Should have been able to read the input file");
    return BufReader::new(input_file);
}

fn add_elf_to_top_stack(new_elf: u32, new_elf_pos: usize, bulk_stack: &mut [u32]) {
    // move every elf after the new elf along by one, starting at the end
    for i in (new_elf_pos + 1..bulk_stack.len()).rev() {
        bulk_stack[i] = bulk_stack[i - 1];
    }
    // insert the new elf at his position
    bulk_stack[new_elf_pos] = new_elf;
}

fn find_three_biggest_elf_bulks(reader: BufReader<File>) -> [u32; 3] {
    /* Calorie total of the top 3 elves, in descending order */
    let mut top_elf_bulks: [u32; 3] = [0; 3];
    let mut cur_elf_bulk: u32 = 0;

    for line_res in reader.lines() {
        let line = line_res.expect("Line could not be read");

        // finished reading an elf - his bulk is over
        if line == "" {
            // check if he was in the top three bulkers (nice) then start the next elf
            for i in 0..top_elf_bulks.len() {
                if cur_elf_bulk >= top_elf_bulks[i] {
                    add_elf_to_top_stack(cur_elf_bulk, i, &mut top_elf_bulks);
                    break;
                }
            }
            cur_elf_bulk = 0;
            continue;
        }
        // otherwise add to the current elf's bulk total
        cur_elf_bulk += line.parse::<u32>().unwrap();
    }
    return top_elf_bulks;
}

fn main() {
    let three_biggest_elves = find_three_biggest_elf_bulks(read_input());
    println!("problem 1: {}", three_biggest_elves[0]);
    println!("problem 2: {}", three_biggest_elves.iter().sum::<u32>());
}
