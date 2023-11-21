use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

mod helpers;

const OUTCOMES_N: u32 = 3;
const DIFF_OPP_LOSES: u32 = 0;
const DIFF_OPP_WINS: u32 = 1;
const DIFF_DRAW: u32 = 2;

const ROCK: u32 = 2;
const PAPER: u32 = 0;
const SCISSORS: u32 = 1;


/* Problem 1 */
fn calculate_score_problem_one(
    line: String,
    shape_points_map: &HashMap<char, u32>,
    outcome_points_map: &HashMap<u32, u32>,
) -> u32 {
    /* Read in the shapes */
    let mut shapes = line.chars();
    let opp_shape = shapes.next().expect("Could not find opponent's shape");
    // skip the space
    shapes.next();
    let my_shape = shapes.next().expect("Could not find my shape");
    /* ------------------ */

    // the outcome is the difference between our play and opponent's, mod 3
    let outcome_id = (my_shape as u32 - opp_shape as u32) % OUTCOMES_N;
    // add the value of the outcome
    let outcome_score = *outcome_points_map
        .get(&outcome_id)
        .expect("Could not find an outcome points value for the calculated outcome id");

    // add the value of our played shape
    let additional_score = *shape_points_map
        .get(&my_shape)
        .expect("Could not find an additional points value for my shape");
    return outcome_score + additional_score;
}

fn get_total_score_one(reader: BufReader<File>) -> u32 {
    // maps from play values to scored points
    let shape_points_map = HashMap::from([('X', 1), ('Y', 2), ('Z', 3)]);
    let outcome_points_map =
        HashMap::from([(DIFF_OPP_WINS, 0), (DIFF_DRAW, 3), (DIFF_OPP_LOSES, 6)]);

    let mut total_score = 0;
    for line_res in reader.lines() {
        let line = line_res.expect("Line could not be read");
        total_score += calculate_score_problem_one(line, &shape_points_map, &outcome_points_map);
    }
    return total_score;
}
/* --------- */


/* Problem 2 */
fn get_shape_for_outcome(opp_shape: char, outcome_id: u32, shapes_ids_map: &HashMap<u32, char>) -> char {
    // determine what shape we need to play to get a desired outcome
    let opp_shape_id = opp_shape as u32 % 3;
    let my_shape_id: u32;
    if outcome_id == DIFF_DRAW {
        my_shape_id = opp_shape_id;
    } else if outcome_id == DIFF_OPP_LOSES {
        my_shape_id = (opp_shape_id + 1) % 3;
    } else {
        my_shape_id = (opp_shape_id + 2) % 3;
    }
    let my_shape = shapes_ids_map
        .get(&my_shape_id)
        .expect("Could not find a shape for the calculated shape id");

    return *my_shape;
}
fn calculate_score_problem_two(
    line: String,
    shapes_ids_map: &HashMap<u32, char>,
    shape_points_map: &HashMap<char, u32>,
    outcome_points_map: &HashMap<u32, u32>,
) -> u32 {
    /* Read in the shapes and outcome */
    let mut chars = line.chars();
    let opp_shape = chars.next().expect("Could not find opponent's shape");
    // skip the space
    chars.next();
    // the outcome is the final letter's value, mod 3
    let outcome_id = chars.next().expect("Could not find my shape") as u32 % OUTCOMES_N;
    let my_shape = get_shape_for_outcome(opp_shape, outcome_id, shapes_ids_map);
    /* ------------------------------ */

    // add the value of the outcome
    let outcome_score = *outcome_points_map
        .get(&outcome_id)
        .expect("Could not find an outcome points value for the calculated outcome id");

    // add the value of our played shape
    let additional_score = *shape_points_map
        .get(&my_shape)
        .expect("Could not find an additional points value for my shape");
    return outcome_score + additional_score;
}

fn get_total_score_two(reader: BufReader<File>) -> u32 {
    let shapes_ids_map = HashMap::from([(ROCK, 'A'), (PAPER, 'B'), (SCISSORS, 'C')]);
    // maps from play values to scored points
    let shape_points_map = HashMap::from([('A', 1), ('B', 2), ('C', 3)]);
    let outcome_points_map =
        HashMap::from([(DIFF_OPP_WINS, 0), (DIFF_DRAW, 3), (DIFF_OPP_LOSES, 6)]);

    let mut total_score = 0;
    for line_res in reader.lines() {
        let line = line_res.expect("Line could not be read");
        total_score += calculate_score_problem_two(
            line,
            &shapes_ids_map,
            &shape_points_map,
            &outcome_points_map,
        );
    }
    return total_score;
}
/* --------- */


fn main() {
    println!("{}", get_total_score_one(helpers::read_input()));
    println!("{}", get_total_score_two(helpers::read_input()));
}
