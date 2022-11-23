use nalgebra::{DMatrix, RowDVector};
use std::fs::File;
use std::io::{BufRead, BufReader, Error};

type M__ = DMatrix<u32>;
type V__ = RowDVector<u32>;

fn read_input(path: &str) -> Result<M__, Error> {
    let file = File::open(path)?;
    let br = BufReader::new(file);
    let matrix_data: Vec<V__> = br
        .lines()
        .map(|l| V__::from_vec(l.unwrap().chars().map(|c| c.to_digit(2).unwrap()).collect()))
        .collect();

    Ok(M__::from_rows(&matrix_data))
}

fn calculate_gamma_and_epsilon(matrix: &M__) -> (String, String) {
    let (height, width) = matrix.shape();
    let mut gamma = String::new();
    let mut epsilon = String::new();

    for i in 0..width {
        let col_sum: usize = matrix.column(i).sum().try_into().unwrap();
        gamma.push_str(if height - col_sum < col_sum { "1" } else { "0" });
        epsilon.push_str(if height - col_sum > col_sum { "1" } else { "0" });
    }
    (gamma, epsilon)
}
fn bit_string_to_int(bit_string: &String) -> usize {
    usize::from_str_radix(bit_string, 2).unwrap()
}

fn problem_1(matrix: &M__) -> usize {
    let (gamma, epsilon) = calculate_gamma_and_epsilon(&matrix);
    bit_string_to_int(&gamma) * bit_string_to_int(&epsilon)
}

//fn get_rating(matrix: &M__, bits: String) -> usize {
//    let (mut height, width) = matrix.shape();
//    let mut proj = M__::zeros(height, width);
//    proj.copy_from(&matrix.rows(0, height));
//    let mut j = 0;
//    while height > 1 {
//        for i in 0..height {
//            // having problems with ownership making something not work here -- have kinda given
//            // up until I can dedicate some time into reading up on this.
//            if proj[(i, j)] == bits.chars().nth(j).unwrap().to_digit(2).unwrap() {
//                proj.remove_row(i);
//                //not implemented, bla bla, probably easy.
//                j += 1;
//                height -= 1;
//            }
//        }
//    }
//    0
//}
//
//fn problem_2(matrix: M__) -> usize {
//    let (gamma, epsilon) = calculate_gamma_and_epsilon(&matrix);
//    let oxy_rating = get_rating(&matrix, gamma);
//    let co2_rating = get_rating(&matrix, epsilon);
//    oxy_rating * co2_rating
//}

fn main() -> Result<(), Error> {
    // Get input
    let test_matrix = read_input("test_input.txt")?;
    let matrix = read_input("input.txt")?;

    let test_solution_1 = problem_1(&test_matrix);
    assert_eq!(test_solution_1, 198);

    let solution_1 = problem_1(&matrix);
    println!("Solution problem 1: {}", solution_1);

    //let test_solution_2 = problem_2(test_matrix);
    //assert_eq!(test_solution_2, 230);

    //let solution_2 = problem_2(matrix);
    //println!("Solution problem 2: {}", solution_2);

    Ok(())
}
