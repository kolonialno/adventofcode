use std::time::Instant;

use nalgebra::DMatrix;

const DAYS: u16 = 256;
fn main() {
    let mut state: [f64; 9] = [0.; 9];
    let matrix: Vec<f64> = vec![
        0., 1., 0., 0., 0., 0., 0., 0., 0.,
        0., 0., 1., 0., 0., 0., 0., 0., 0.,
        0., 0., 0., 1., 0., 0., 0., 0., 0.,
        0., 0., 0., 0., 1., 0., 0., 0., 0.,
        0., 0., 0., 0., 0., 1., 0., 0., 0.,
        0., 0., 0., 0., 0., 0., 1., 0., 0.,
        1., 0., 0., 0., 0., 0., 0., 1., 0.,
        0., 0., 0., 0., 0., 0., 0., 0., 1.,
        1., 0., 0., 0., 0., 0., 0., 0., 0.,
    ];

    include_str!("../input.txt")
        .split(",")
        .map(|f| f.parse::<usize>().unwrap())
        .for_each(|f| state[f] += 1.);

    let mut state_array = state.clone();

    //ARRAY
    let start_array = Instant::now();
    for _ in 0..DAYS {
        state_array.rotate_left(1);
        state_array[6] += state_array[8];
    }

    println!("answer from array: {}", state_array.iter().sum::<f64>());
    let duration_array = start_array.elapsed();
    println!("duration with array: {:?}", duration_array);

    //MATRIX
    let start_matrix = Instant::now();

    let flipper_m = DMatrix::from_row_slice(9, 9, &matrix)
        .pow(DAYS - 1)
        .unwrap();
    let init_state = DMatrix::from_vec(9, 1, Vec::from(state));

    let duration_matrix = start_matrix.elapsed();

    println!(
        "answer from matrix: {:?}",
        (&flipper_m * &init_state).iter().sum::<f64>()
    );
    println!("duration with matrix: {:?}", duration_matrix);
}
