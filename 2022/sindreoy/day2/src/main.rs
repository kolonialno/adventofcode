fn main() {
    let input_string = include_str!("../input.txt");
    let (opponent_moves, best_responses) = get_all_moves(input_string);
    assert_eq!(opponent_moves.len(), best_responses.len());

    // Denote how to interpret second column
    let option = 2;
    let score_vec = calc_score_all_moves(opponent_moves, best_responses, option);
    let total_score_all_moves = calc_total_score_of_all_moves(score_vec);
    println!("{}", total_score_all_moves);
}

fn get_one_move(line: &str) -> (&str, &str) {
    let line_vec: Vec<&str> = line.split_whitespace().collect();
    return (line_vec[0], line_vec[1]);
}

fn get_all_moves(input_string: &str) -> (Vec<&str>, Vec<&str>) {
    let mut opponent_moves: Vec<&str> = Vec::new();
    let mut best_responses: Vec<&str> = Vec::new();
    for line in input_string.lines() {
        let (opponent_move, best_response) = get_one_move(line);
        opponent_moves.push(opponent_move);
        best_responses.push(best_response);
    }
    return (opponent_moves, best_responses);
}

fn calc_score_of_move(opponent_move: &str, best_response: &str) -> i32 {
    // A, X : Rock
    // B, Y : Paper
    // C, Z : Scissors
    let (move_score, win_score): (i32, i32) = match (opponent_move, best_response) {
        ("A", "X") => (1, 3), // Draw
        ("A", "Y") => (2, 6), // Win
        ("A", "Z") => (3, 0), // Loss
        ("B", "X") => (1, 0), // Loss
        ("B", "Y") => (2, 3), // Draw
        ("B", "Z") => (3, 6), // Win
        ("C", "X") => (1, 6), // Win
        ("C", "Y") => (2, 0), // Loss
        ("C", "Z") => (3, 3), // Draw
        _ => (0, 0), // Got crap input, no score
    };

    return move_score + win_score;
}

fn calc_score_of_cryptic_move(opponent_move: &str, best_response: &str) -> i32 {
    // A, X : Rock, Lose
    // B, Y : Paper, Draw
    // C, Z : Scissors, Win
    let (move_score, win_score): (i32, i32) = match (opponent_move, best_response) {
        ("A", "X") => (3, 0), // Loss, play scissors
        ("A", "Y") => (1, 3), // Draw, play rock
        ("A", "Z") => (2, 6), // Win,  play paper
        ("B", "X") => (1, 0), // Loss, play rock
        ("B", "Y") => (2, 3), // Draw, play paper
        ("B", "Z") => (3, 6), // Win,  play scissors
        ("C", "X") => (2, 0), // Loss, play paper
        ("C", "Y") => (3, 3), // Draw, play scissors
        ("C", "Z") => (1, 6), // Win,  play rock
        _ => (0, 0), // Got crap input, no score
    };

    return move_score + win_score;
}

fn calc_score_all_moves(opponent_moves: Vec<&str>, best_responses: Vec<&str>, option: i32) -> Vec<i32> {
    assert_eq!(opponent_moves.len(), best_responses.len());
    let mut score_vec: Vec<i32> = Vec::new();

    let num_games: usize = opponent_moves.len();
    for i in 0..num_games {
        let score_of_move = match option {
            1 => calc_score_of_move(opponent_moves[i], best_responses[i]),
            2 => calc_score_of_cryptic_move(opponent_moves[i], best_responses[i]),
            _ => 0
        };
        score_vec.push(score_of_move);
    }
    return score_vec;
}

fn calc_total_score_of_all_moves(score_vec: Vec<i32>) -> i32 {
    return score_vec.iter().sum();
}