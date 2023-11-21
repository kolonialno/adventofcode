type Result<T> = std::result::Result<T, anyhow::Error>;

#[derive(PartialEq, Copy, Clone)]
enum Symbol {
    Rock,
    Paper,
    Scissors,
}

enum Outcome {
    Loss,
    Tie,
    Win,
}

fn symbol_that_wins_against(x: Symbol) -> Symbol {
    match x {
        Symbol::Rock => Symbol::Paper,
        Symbol::Paper => Symbol::Scissors,
        Symbol::Scissors => Symbol::Rock,
    }
}

fn symbol_that_loses_against(x: Symbol) -> Symbol {
    match x {
        Symbol::Paper => Symbol::Rock,
        Symbol::Scissors => Symbol::Paper,
        Symbol::Rock => Symbol::Scissors,
    }
}

fn outcome_for_me(me: Symbol, them: Symbol) -> Outcome {
    if symbol_that_wins_against(me) == them {
        Outcome::Loss
    } else if symbol_that_wins_against(them) == me {
        Outcome::Win
    } else {
        Outcome::Tie
    }
}

fn symbol_to_choose(opponent: Symbol, desired_outcome: Outcome) -> Symbol {
    match desired_outcome {
        Outcome::Tie => opponent,
        Outcome::Win => symbol_that_wins_against(opponent),
        Outcome::Loss => symbol_that_loses_against(opponent),
    }
}

fn symbol_score(x: Symbol) -> i32 {
    match x {
        Symbol::Rock => 1,
        Symbol::Paper => 2,
        Symbol::Scissors => 3,
    }
}

fn round_score(me: Symbol, them: Symbol) -> i32 {
    symbol_score(me)
        + match outcome_for_me(me, them) {
            Outcome::Win => 6,
            Outcome::Tie => 3,
            Outcome::Loss => 0,
        }
}

fn main() -> Result<()> {
    let mut running_total_interpretation_one: i32 = 0;
    let mut running_total_interpretation_two: i32 = 0;

    for line in std::io::stdin().lines() {
        let line = line?;
        let (them, second_column) = line.split_once(' ').unwrap();
        let them = match them {
            "A" => Symbol::Rock,
            "B" => Symbol::Paper,
            "C" => Symbol::Scissors,
            _ => panic!("bad symbol for them"),
        };

        let interpretation_one_me = match second_column {
            "X" => Symbol::Rock,
            "Y" => Symbol::Paper,
            "Z" => Symbol::Scissors,
            _ => panic!("bad symbol for me"),
        };

        let interpretation_two_desired_outcome = match second_column {
            "X" => Outcome::Loss,
            "Y" => Outcome::Tie,
            "Z" => Outcome::Win,
            _ => panic!("bad symbol for desired outcome"),
        };

        let interpretation_two_me = symbol_to_choose(them, interpretation_two_desired_outcome);

        running_total_interpretation_one += round_score(interpretation_one_me, them);
        running_total_interpretation_two += round_score(interpretation_two_me, them);
    }

    println!(
        "Total score (interpretation one): {}",
        running_total_interpretation_one
    );
    println!(
        "Total score (interpretation two): {}",
        running_total_interpretation_two
    );

    Ok(())
}
