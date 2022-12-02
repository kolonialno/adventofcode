
#[derive(Clone, Copy, Debug, PartialEq)]
enum Hand {
    ROCK = 1,
    PAPER = 2,
    SCISSORS = 3,
}

impl Hand {
    fn from(s: &str) -> Hand {
        match s {
            "A" | "X"=> Hand::ROCK,
            "B" | "Y" => Hand::PAPER,
            "C" | "Z" => Hand::SCISSORS,
            _ => panic!("Hand::from(\"{}\")", s)
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum RoundResult {
    LOSS = 0,
    DRAW = 3,
    WIN = 6,
}

type Goal = RoundResult;

impl Goal {
    fn from(s: &str) -> Goal {
        match s {
            "X" => Goal::LOSS,
            "Y" => Goal::DRAW,
            "Z" => Goal::WIN,
            _ => panic!("Hand::from(\"{}\")", s)
        }
    }
}


#[derive(Clone, Copy, Debug, PartialEq)]
struct RoundP1 {
    own: Hand,
    opponent: Hand,
}

impl RoundP1 {
    fn play(self) -> RoundResult {
        match (self.own, self.opponent) {
            (Hand::ROCK, Hand::SCISSORS) | (Hand::PAPER, Hand::ROCK) | (Hand::SCISSORS, Hand::PAPER) => RoundResult::WIN,
            (Hand::SCISSORS, Hand::ROCK) | (Hand::ROCK, Hand::PAPER) | (Hand::PAPER, Hand::SCISSORS) => RoundResult::LOSS,
            _ => RoundResult::DRAW,
        }
    }
    fn score(self) -> u64 {
        self.play() as u64 + self.own as u64
    }
}


#[derive(Clone, Copy, Debug, PartialEq)]
struct RoundWithGoal {
    opponent: Hand,
    goal: Goal,
}

impl RoundWithGoal {
    fn get_own_hand_for_result(self, r: Goal) -> Hand {
        match r {
            Goal::WIN => 
                match self.opponent {
                    Hand::ROCK => Hand::PAPER,
                    Hand::PAPER => Hand::SCISSORS,
                    Hand::SCISSORS => Hand::ROCK,
                },
            Goal::LOSS =>
                match self.opponent {
                    Hand::ROCK => Hand::SCISSORS,
                    Hand::PAPER => Hand::ROCK,
                    Hand::SCISSORS => Hand::PAPER,
                },
            _ => self.opponent
        }
    }
}



fn parse(data: &str) -> Vec<&str> {
    data
    .split("\n")
    .filter(|l| !l.is_empty())
    .collect() 
}

fn solve1(data: &str) -> u64 {
    let rounds: Vec<RoundP1> = parse(data).iter()
        .map(|l| l.split(" ").map(Hand::from).collect())
        .map(|r: Vec<Hand>| RoundP1 { opponent: r[0], own: r[1]})
        .collect(); 
    rounds.iter().map(|r| r.score()).sum()
}

fn solve2(data: &str) -> u64 {
    let round_goals: Vec<RoundWithGoal> = parse(data).iter()
        .map(|l| l.split(" ").collect())
        .map(|l: Vec<&str>| RoundWithGoal {opponent: Hand::from(l[0]), goal: Goal::from(l[1])}) 
        .collect();
    
    let rounds = round_goals.iter()
        .map(|rg| RoundP1 { opponent: rg.opponent, own: rg.get_own_hand_for_result(rg.goal)});
    
    rounds.map(|r| r.score()).sum()
    
}


fn main() {
    let data = include_str!("../input.txt");
    println!("Part 1: {}", solve1(data));
    println!("Part 2: {}", solve2(data));
}

#[test]
fn test_part1() {
    let data = include_str!("../example.txt");
    assert_eq!(solve1(data), 15)
}

#[test]
fn test_part2() {
    let data = include_str!("../example.txt");
    assert_eq!(solve2(data), 12)
}