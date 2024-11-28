fn main() {
    let input_string = include_str!("../input.txt");
    let parts: Vec<&str> = input_string.split("\n\n").collect();
    let initial_stack_string = parts[0];
    let instructions_string = parts[1];

    let mut initial_stacks = create_initial_stacks(initial_stack_string);
    let mutable_stacks = &mut initial_stacks;
    rearrange_all_stacks(mutable_stacks, instructions_string);
    println!("{:?}", mutable_stacks);

    let num_stacks = get_number_of_stacks(input_string);
    for i in 0..num_stacks {
        println!("{:?}", mutable_stacks[i as usize].last().unwrap());
    }
}

fn get_number_of_stacks (initial_stack_string: &str) -> u16 {
    let mut counter: u16 = 0;
    for line in initial_stack_string.lines() {
        for c in line.chars() {
            if c.is_digit(10) {
                let c_digit = match c.to_digit(10) {
                    Some(number) => number,
                    None => 0,
                } as u16;
                if c_digit > counter {
                    counter = c_digit;
                }
            }
        }
    }
    return counter;
}

fn create_initial_stacks (initial_stack_string: &str) -> Vec<Vec<char>> {
    let num_stacks = get_number_of_stacks(initial_stack_string);
    let mut stack_vec = Vec::new();
    for _ in 0..num_stacks {
        stack_vec.push(Vec::new());
    }
    for line in initial_stack_string.lines() {
        let mut i = 0;
        let mut vector_counter = 0;
        let mut iter = line.chars();
        while i < num_stacks*4 {
            let c = match iter.next() {
                Some(character) => character,
                None => ' ',
            };
            if c.is_alphabetic() {
                stack_vec[vector_counter].push(c);
            }
            if i % 4 == 0 && i != 0 {
                vector_counter += 1;
            }
            i += 1;
        }
    }
    for idx in 0..num_stacks {
        stack_vec[idx as usize].reverse();
    }
    return stack_vec;
}

fn rearrange_all_stacks(initial_stack_vector: &mut Vec<Vec<char>>, instructions: &str) {
    for instruction in instructions.lines() {
        let (num_ops, from, to) = process_one_instruction(instruction);
        for _ in 0..num_ops {
            rearrange_one_stack_operation(initial_stack_vector, num_ops, from, to);
        }
    }
}

fn rearrange_one_stack_operation(stack_vector: &mut Vec<Vec<char>>, num_ops: u16, from: usize, to: usize) {
    let value = match stack_vector[from].last().cloned() {
        Some(character) => character,
        None => panic!("Empty"),
    };
    stack_vector[from].pop();
    stack_vector[to].push(value);
}

fn process_one_instruction (instruction: &str) -> (u16, usize, usize) {
    let mut num_vec = Vec::new();
    let words: Vec<&str> = instruction.split_whitespace().collect();
    for word in words {
        let mut should_add = true;
        for c in word.chars() {
            if !c.is_digit(10) {
                should_add = false;
            }
        }
        if should_add == true {
            num_vec.push(u16::from_str_radix(word, 10).unwrap_or(0));
        }
    }

    let num_ops = num_vec[0] as u16;
    let from = (num_vec[1] - 1) as usize;
    let to = (num_vec[2] - 1) as usize;
    return (num_ops, from, to);
}