use regex::Regex;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::io::Read;

type Result<T> = std::result::Result<T, anyhow::Error>;

const MAX_NODES: usize = 51;
const MAX_NUMBER_OF_MINUTES: usize = 30;

struct Node {
    paths: Vec<usize>,
    rate: i64,
    valve_index: Option<usize>,
}

type ValveStates = u16;
type CachedScores = [Option<i64>; MAX_NUMBER_OF_MINUTES];

struct ScoreMemo {
    memo: HashMap<((usize, usize), ValveStates), CachedScores>,
}

impl ScoreMemo {
    fn new() -> ScoreMemo {
        ScoreMemo {
            memo: HashMap::new(),
        }
    }

    fn update(&mut self, state: &State) -> bool {
        let (p0, p1) = state.positions;
        let canonical_pos = (p0.min(p1), p0.max(p1));
        let key = (canonical_pos, state.encoded_on);

        let value: &mut [Option<i64>; MAX_NUMBER_OF_MINUTES] = self
            .memo
            .entry(key)
            .or_insert_with(|| [None; MAX_NUMBER_OF_MINUTES]);

        for previous_score in value.iter().take(state.time + 1).flatten() {
            if *previous_score >= state.score {
                return false;
            }
        }

        value[state.time] = Some(state.score);
        true
    }
}

#[derive(Debug)]
struct State {
    time: usize,
    positions: (usize, usize),
    encoded_on: ValveStates,
    score: i64,
    use_elephant: bool,
}

impl State {
    fn initial_state(starting_position: usize, use_elephant: bool) -> State {
        State {
            time: if use_elephant { 4 } else { 0 },
            positions: (starting_position, starting_position),
            encoded_on: 0,
            score: 0,
            use_elephant,
        }
    }

    fn move_is_pointless(
        &self,
        nodes: &[Node],
        distances: &[Vec<Option<i32>>],
        before: usize,
        after: usize,
        time_left: usize,
    ) -> bool {
        // This optimization doesn't actually seem to matter much.
        !nodes.iter().enumerate().any(|(i, node)| {
            if let Some(valve_index) = node.valve_index {
                let bit = (1 as ValveStates) << valve_index;
                if (self.encoded_on & bit) != 0 {
                    return false;
                }

                let distance_before = distances[before][i].unwrap();
                let distance_after = distances[after][i].unwrap();
                let progress = distance_before - distance_after;
                let max_steps_left = time_left - 1; // Also need 1min to toggle.
                let has_enough_time = distance_before <= max_steps_left.try_into().unwrap();
                progress > 0 && has_enough_time
            } else {
                false
            }
        })
    }

    fn get_valve_on_at(&self, nodes: &[Node], node_index: usize) -> bool {
        match nodes[node_index].valve_index {
            None => false,
            Some(i) => {
                let bit = (1 as ValveStates) << i;
                (self.encoded_on & bit) != 0
            }
        }
    }

    fn minutes_left(&self) -> usize {
        MAX_NUMBER_OF_MINUTES - (self.time + 1)
    }

    fn upper_bound_for_score(&self, nodes: &[Node], distance_matrix: &[Vec<Option<i32>>]) -> i64 {
        let minutes_left: i64 = self.minutes_left() as i64;

        let upper_bound_extra_score = nodes
            .iter()
            .enumerate()
            .filter_map(|(i, node)| {
                node.valve_index?;

                let valve_index = node.valve_index.unwrap();
                let bit: ValveStates = 1 << valve_index;
                if (bit & self.encoded_on) != 0 {
                    return None;
                }

                let distance = distance_matrix[self.positions.0][i]
                    .unwrap()
                    .min(distance_matrix[self.positions.1][i].unwrap());

                let minutes_flowing = minutes_left - (distance as i64);

                if minutes_flowing <= 0 {
                    return None;
                }

                Some(nodes[i].rate * minutes_flowing)
            })
            .sum::<i64>();
        assert!(upper_bound_extra_score >= 0);

        upper_bound_extra_score + self.score
    }

    fn next_states(&self, nodes: &[Node], distance_matrix: &[Vec<Option<i32>>]) -> Vec<State> {
        assert!(nodes.len() <= MAX_NODES);
        assert!(self.time < MAX_NUMBER_OF_MINUTES);

        let my_node = &nodes[self.positions.0];
        let its_node = &nodes[self.positions.1];

        let minutes_left_to_flow_after_current_turn = MAX_NUMBER_OF_MINUTES - (self.time + 1);

        let mut rv: Vec<State> = Vec::new();

        let mut my_possible_actions: Vec<(usize, i64)> = Vec::new();
        let mut its_possible_actions: Vec<(usize, i64)> = Vec::new();

        for my_next_node in &my_node.paths {
            let pointless = self.move_is_pointless(
                nodes,
                distance_matrix,
                self.positions.0,
                *my_next_node,
                minutes_left_to_flow_after_current_turn,
            );
            if !pointless {
                my_possible_actions.push((*my_next_node, 0));
            }
        }

        for its_next_node in &its_node.paths {
            let pointless = self.move_is_pointless(
                nodes,
                distance_matrix,
                self.positions.1,
                *its_next_node,
                minutes_left_to_flow_after_current_turn,
            );

            if !pointless {
                its_possible_actions.push((*its_next_node, 0));
            }
        }

        if my_possible_actions.is_empty() {
            my_possible_actions.push((self.positions.0, 0));
        }

        if its_possible_actions.is_empty() {
            its_possible_actions.push((self.positions.1, 0));
        }

        if my_node.rate > 0 && !self.get_valve_on_at(nodes, self.positions.0) {
            let extra_score = my_node.rate * (minutes_left_to_flow_after_current_turn as i64);
            my_possible_actions.push((self.positions.0, extra_score));
        }

        if its_node.rate > 0 && !self.get_valve_on_at(nodes, self.positions.1) {
            let extra_score = its_node.rate * (minutes_left_to_flow_after_current_turn as i64);
            its_possible_actions.push((self.positions.1, extra_score));
        }

        if !self.use_elephant {
            its_possible_actions = vec![(0, 0)];
        }

        for (my_next_pos, my_extra_score) in &my_possible_actions {
            for (its_next_pos, its_extra_score) in &its_possible_actions {
                let p0: usize = *my_next_pos;
                let p1: usize = *its_next_pos;

                if p0 == p1 && *my_extra_score > 0 && *its_extra_score > 0 {
                    // We can't both flip the same valve.
                    continue;
                }

                let mut new_on = self.encoded_on;

                if *my_extra_score > 0 {
                    let bit = (1 as ValveStates) << my_node.valve_index.unwrap();
                    assert!((new_on & bit) == 0);
                    new_on |= bit;
                    assert!((new_on & bit) != 0);
                }

                if *its_extra_score > 0 {
                    let bit = (1 as ValveStates) << its_node.valve_index.unwrap();
                    assert!((new_on & bit) == 0);
                    new_on |= bit;
                    assert!((new_on & bit) != 0);
                }

                rv.push(State {
                    time: self.time + 1,
                    positions: (p0, p1),
                    encoded_on: new_on,
                    score: self.score + my_extra_score + its_extra_score,
                    use_elephant: self.use_elephant,
                });
            }
        }

        rv
    }
}

fn solve(nodes: &[Node], starting_position: usize, use_elephant: bool) -> Result<i64> {
    let distances = distance_matrix(nodes);

    let mut iteration_count: u64 = 0;

    let mut max_observed_score: i64 = 0;

    let mut memo = ScoreMemo::new();

    let mut q = VecDeque::new();
    q.push_back(State::initial_state(starting_position, use_elephant));

    let starting_time = q[0].time;

    while !q.is_empty() {
        iteration_count += 1;

        let state = q.pop_back().unwrap();

        max_observed_score = state.score.max(max_observed_score);

        let score_upper_bound = state.upper_bound_for_score(nodes, &distances);
        if score_upper_bound < max_observed_score {
            continue;
        }

        if iteration_count % 100_000 == 0 {
            eprintln!(
                "Iteration {}; queue size {}, time {}, cur score {}, upper-bound: {}, max score {}",
                iteration_count,
                q.len(),
                state.time,
                state.score,
                score_upper_bound,
                max_observed_score,
            );
        }

        if state.time >= (MAX_NUMBER_OF_MINUTES - 1) {
            continue;
        }

        for new_state in state.next_states(nodes, &distances) {
            if memo.update(&new_state) {
                q.push_back(new_state);
            }
        }
    }

    eprintln!("Done after {} iterations.", iteration_count);
    eprintln!(
        "Max score (with elephant? {}) after {} minutes: {}",
        use_elephant,
        MAX_NUMBER_OF_MINUTES - starting_time,
        max_observed_score
    );

    Ok(max_observed_score)
}

fn distances_from(nodes: &[Node], start: usize) -> Vec<Option<i32>> {
    let n = nodes.len();
    let mut rv: Vec<Option<i32>> = std::iter::repeat(None).take(n).collect();
    let mut visited: Vec<bool> = std::iter::repeat(false).take(n).collect();
    let mut q = VecDeque::new();

    q.push_back((0, start));
    visited[start] = true;

    while let Some((distance, index)) = q.pop_front() {
        rv[index] = Some(distance);

        for next_index in &nodes[index].paths {
            if !visited[*next_index] {
                visited[*next_index] = true;
                q.push_back((distance + 1, *next_index));
            }
        }
    }

    rv
}

fn distance_matrix(nodes: &[Node]) -> Vec<Vec<Option<i32>>> {
    (0..nodes.len()).map(|i| distances_from(nodes, i)).collect()
}

fn parse_scenario(s: String) -> Result<(Vec<Node>, usize)> {
    let re = Regex::new(
        r"Valve (?P<name>[A-Z0-9]+) has flow rate=(?P<rate>[0-9]+); tunnels? leads? to valves? (?P<out>[A-Z0-9, ]+)",
    )?;

    let lines: Vec<String> = s.trim().split('\n').map(|line| line.to_string()).collect();

    let mut name_to_index: HashMap<String, usize> = HashMap::new();
    let mut nodes: Vec<(i64, Vec<String>)> = Vec::new();

    for line in lines {
        let captures = re.captures(line.trim()).unwrap();
        let name: String = captures["name"].to_string();
        let rate: i64 = captures["rate"].parse().unwrap();
        let out: Vec<String> = captures["out"]
            .split(',')
            .map(|x| x.trim().to_string())
            .collect();
        let index: usize = nodes.len();
        name_to_index.insert(name, index);
        nodes.push((rate, out));
    }

    let mut current_valve_index: usize = 0;

    let nodes: Vec<Node> = nodes
        .iter()
        .map(|(rate, out_names)| {
            let rate = *rate;
            let paths: Vec<usize> = out_names
                .iter()
                .map(|name| *name_to_index.get(name).unwrap())
                .collect();
            let valve_index = if rate > 0 {
                let vi = current_valve_index;
                assert!(vi <= 15);
                current_valve_index += 1;
                Some(vi)
            } else {
                None
            };
            Node {
                paths,
                rate,
                valve_index,
            }
        })
        .collect();

    let starting_position: usize = name_to_index["AA"];

    Ok((nodes, starting_position))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_test(s: &str, with_elephant: bool) -> Result<i64> {
        let (nodes, starting_position) = parse_scenario(s.to_string())?;
        solve(&nodes, starting_position, with_elephant)
    }

    #[test]
    fn simple_scenario_with_elephants() {
        assert_eq!(
            run_test(
                r#"
Valve AA has flow rate=10; tunnels lead to valves BB, CC
Valve BB has flow rate=100; tunnels lead to valves AA
Valve CC has flow rate=100; tunnels lead to valves AA
"#,
                true
            )
            .unwrap(),
            5020,
        );
    }

    #[test]
    fn simple_scenario_without_elephants() {
        assert_eq!(
            run_test(
                r#"
Valve AA has flow rate=10; tunnels lead to valves BB, CC
Valve BB has flow rate=100; tunnels lead to valves AA
Valve CC has flow rate=100; tunnels lead to valves AA
"#,
                false
            )
            .unwrap(),
            5530,
        );
    }

    #[test]
    fn trivial_scenario() {
        assert_eq!(
            run_test(
                r#"
Valve AA has flow rate=1; tunnels lead to valves BB
Valve BB has flow rate=0; tunnels lead to valves AA
"#,
                false
            )
            .unwrap(),
            29,
        );
    }

    #[test]
    fn trivial_scenario_with_elephant() {
        assert_eq!(
            run_test(
                r#"
Valve AA has flow rate=1; tunnels lead to valves BB
Valve BB has flow rate=0; tunnels lead to valves AA
"#,
                true
            )
            .unwrap(),
            25,
        );
    }

    #[test]
    fn scenario_from_question_text_part_one() {
        assert_eq!(
            run_test(
                r#"
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"#,
                false
            )
            .unwrap(),
            1651,
        );
    }

    #[test]
    fn scenario_from_question_text_part_two() {
        assert_eq!(
            run_test(
                r#"
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"#,
                true
            )
            .unwrap(),
            1707,
        );
    }

    const REAL_INPUT: &str = r#"
Valve JC has flow rate=0; tunnels lead to valves XS, XK
Valve TK has flow rate=0; tunnels lead to valves AA, RA
Valve PY has flow rate=0; tunnels lead to valves UB, MW
Valve XK has flow rate=15; tunnels lead to valves CD, JC, TP, UE
Valve EI has flow rate=6; tunnels lead to valves UB, HD
Valve OV has flow rate=0; tunnels lead to valves QC, WK
Valve CX has flow rate=3; tunnels lead to valves ZN, AM, OE, YS, QE
Valve YS has flow rate=0; tunnels lead to valves QC, CX
Valve DC has flow rate=0; tunnels lead to valves UE, NM
Valve EA has flow rate=5; tunnels lead to valves QE, XO, GX
Valve VE has flow rate=0; tunnels lead to valves YH, NM
Valve RN has flow rate=0; tunnels lead to valves WK, NU
Valve VJ has flow rate=0; tunnels lead to valves QC, CS
Valve HD has flow rate=0; tunnels lead to valves JI, EI
Valve UB has flow rate=0; tunnels lead to valves EI, PY
Valve XS has flow rate=17; tunnels lead to valves JC, CE
Valve AM has flow rate=0; tunnels lead to valves NU, CX
Valve GX has flow rate=0; tunnels lead to valves EA, RA
Valve UI has flow rate=0; tunnels lead to valves NC, ZG
Valve NM has flow rate=22; tunnels lead to valves DC, VE, DX
Valve CE has flow rate=0; tunnels lead to valves XS, WD
Valve NC has flow rate=25; tunnels lead to valves UI, VQ
Valve TP has flow rate=0; tunnels lead to valves XK, RA
Valve ZN has flow rate=0; tunnels lead to valves CX, XI
Valve CS has flow rate=0; tunnels lead to valves AA, VJ
Valve MW has flow rate=23; tunnel leads to valve PY
Valve AA has flow rate=0; tunnels lead to valves TK, WC, CS, AL, MS
Valve RA has flow rate=4; tunnels lead to valves WD, TP, TK, GX, JI
Valve NU has flow rate=10; tunnels lead to valves DU, AM, RN, HS, AL
Valve QE has flow rate=0; tunnels lead to valves CX, EA
Valve AH has flow rate=0; tunnels lead to valves WK, MS
Valve YH has flow rate=20; tunnels lead to valves VE, CD
Valve SH has flow rate=0; tunnels lead to valves DU, ZG
Valve OE has flow rate=0; tunnels lead to valves WC, CX
Valve XO has flow rate=0; tunnels lead to valves EA, ZG
Valve JI has flow rate=0; tunnels lead to valves RA, HD
Valve XI has flow rate=0; tunnels lead to valves WK, ZN
Valve HS has flow rate=0; tunnels lead to valves QC, NU
Valve VQ has flow rate=0; tunnels lead to valves WK, NC
Valve UE has flow rate=0; tunnels lead to valves XK, DC
Valve YP has flow rate=19; tunnel leads to valve DX
Valve WD has flow rate=0; tunnels lead to valves CE, RA
Valve DX has flow rate=0; tunnels lead to valves NM, YP
Valve ZG has flow rate=11; tunnels lead to valves UI, SH, XO
Valve MS has flow rate=0; tunnels lead to valves AA, AH
Valve QC has flow rate=9; tunnels lead to valves HS, VJ, OV, YS
Valve DU has flow rate=0; tunnels lead to valves NU, SH
Valve WK has flow rate=12; tunnels lead to valves RN, XI, VQ, OV, AH
Valve CD has flow rate=0; tunnels lead to valves YH, XK
Valve AL has flow rate=0; tunnels lead to valves AA, NU
Valve WC has flow rate=0; tunnels lead to valves OE, AA
"#;

    #[test]
    fn real_input_part_one() {
        assert_eq!(run_test(REAL_INPUT, false).unwrap(), 1754);
    }

    #[test]
    fn real_input_part_two() {
        // This one can be kind of slow.
        assert_eq!(run_test(REAL_INPUT, true).unwrap(), 2474);
    }

    #[test]
    fn test_distance_map() {
        let (nodes, _) = parse_scenario(REAL_INPUT.to_string()).unwrap();
        let distances = distance_matrix(&nodes);
        let n = nodes.len();

        for i in 0..n {
            assert_eq!(distances[i][i], Some(0));

            for j in 0..n {
                assert_eq!(distances[i][j], distances[j][i]);

                for k in 0..n {
                    assert!(
                        distances[i][k].unwrap() + distances[k][j].unwrap()
                            >= distances[i][j].unwrap()
                    );
                }
            }
        }
    }
}

fn main() -> Result<()> {
    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s)?;

    let (nodes, starting_position) = parse_scenario(s)?;

    println!(
        "Answer to first question: {}",
        solve(&nodes, starting_position, false)?
    );
    println!(
        "Answer to second question: {}",
        solve(&nodes, starting_position, true)?
    );

    Ok(())
}
