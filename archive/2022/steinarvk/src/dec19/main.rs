use regex::Regex;
use std::collections::HashMap;
use std::io::Read;

type Result<T> = std::result::Result<T, anyhow::Error>;

const NUM_RESOURCES: usize = 4;
const OBSIDIAN: usize = 2;
const GEODES: usize = 3;

type ResourceVector = [i64; NUM_RESOURCES];
type RobotVector = [i64; NUM_RESOURCES];
type Blueprint = [ResourceVector; NUM_RESOURCES];

fn add(a: &ResourceVector, b: &ResourceVector) -> ResourceVector {
    let mut rv: ResourceVector = *a;
    for i in 0..NUM_RESOURCES {
        rv[i] += b[i];
    }
    rv
}

fn can_afford(resources: &ResourceVector, cost: &ResourceVector) -> bool {
    for i in 0..NUM_RESOURCES {
        if resources[i] < cost[i] {
            return false;
        }
    }
    true
}

#[derive(Debug, Clone)]
struct State {
    robots: [i64; NUM_RESOURCES],
    resources: ResourceVector,
    minutes_passed: i32,
    minutes_target: i32,
}

impl State {
    fn initial_state(target: i32) -> State {
        State {
            robots: [1, 0, 0, 0],
            resources: [1, 0, 0, 0],
            minutes_passed: 1,
            minutes_target: target,
        }
    }

    fn minutes_left(&self) -> i32 {
        self.minutes_target - self.minutes_passed
    }

    fn geodes(&self) -> i64 {
        self.resources[GEODES]
    }

    fn compute_final_geodes_shortcut(&self, blueprint: &Blueprint) -> Option<i64> {
        let minutes_left = self.minutes_left();
        let geodes = self.geodes();

        if minutes_left == 2 {
            let extra_bots = if can_afford(&self.resources, &blueprint[3]) {
                1
            } else {
                0
            };
            let geodes = geodes + 2 * self.robots[GEODES] + extra_bots;
            return Some(geodes);
        }

        if minutes_left == 1 {
            let geodes = geodes + self.robots[GEODES];
            return Some(geodes);
        }

        if minutes_left == 0 {
            return Some(geodes);
        }

        None
    }

    fn next_dream_state(&self, blueprint: &Blueprint) -> State {
        let i = if can_afford(&self.resources, &blueprint[0]) {
            1
        } else {
            0
        };
        let j = if can_afford(&self.resources, &blueprint[1]) {
            1
        } else {
            0
        };
        let k = if can_afford(&self.resources, &blueprint[2]) {
            1
        } else {
            0
        };
        let l = if can_afford(&self.resources, &blueprint[3]) {
            1
        } else {
            0
        };

        let new_robots: [i64; NUM_RESOURCES] = [i, j, k, l];

        State {
            robots: add(&new_robots, &self.robots),
            resources: add(&self.resources, &self.robots),
            minutes_passed: self.minutes_passed + 1,
            minutes_target: self.minutes_target,
        }
    }

    fn upper_bound_geodes(&self, blueprint: &Blueprint) -> i64 {
        let mut current = self.clone();
        while current.minutes_passed < self.minutes_target {
            if let Some(geodes) = current.compute_final_geodes_shortcut(blueprint) {
                return geodes;
            }
            current = current.next_dream_state(blueprint);
        }
        current.compute_final_geodes_shortcut(blueprint).unwrap()
    }

    fn foreach_next_state<F>(&self, blueprint: &Blueprint, mut callback: F)
    where
        F: FnMut(&State),
    {
        let remaining_resources = self.resources;
        let mut count = 0;

        for (i, bp) in blueprint.iter().enumerate().rev() {
            if can_afford(&self.resources, bp) {
                let mut new_robots = self.robots;
                let mut new_resources = self.resources;

                new_robots[i] += 1;
                bp.iter().enumerate().for_each(|(j, cost)| {
                    new_resources[j] -= cost;
                });

                count += 1;
                callback(&State {
                    robots: new_robots,
                    resources: add(&new_resources, &self.robots),
                    minutes_passed: self.minutes_passed + 1,
                    minutes_target: self.minutes_target,
                });
            }
        }

        let relevant_robots = if self.robots[OBSIDIAN] == 0 { 3 } else { 4 };

        if count < relevant_robots {
            callback(&State {
                robots: self.robots,
                resources: add(&remaining_resources, &self.robots),
                minutes_passed: self.minutes_passed + 1,
                minutes_target: self.minutes_target,
            });
        }
    }
}

struct Memo {
    memo: HashMap<(ResourceVector, RobotVector, i32), i64>,
    iter: i64,
    max_geodes_ever_seen: i64,
    evaluated_for_time: i32,
}

impl Memo {
    fn new() -> Memo {
        Memo {
            memo: HashMap::new(),
            iter: 0,
            max_geodes_ever_seen: 0,
            evaluated_for_time: 0,
        }
    }

    fn evaluate_from(&mut self, blueprint: &Blueprint, state: &State) -> Option<i64> {
        self.iter += 1;

        if let Some(geodes) = state.compute_final_geodes_shortcut(blueprint) {
            self.max_geodes_ever_seen = self.max_geodes_ever_seen.max(geodes);
            return Some(geodes);
        }

        let geodes = state.geodes();
        self.max_geodes_ever_seen = self.max_geodes_ever_seen.max(geodes);

        let upper_bound = state.upper_bound_geodes(blueprint);
        if upper_bound < self.max_geodes_ever_seen {
            return None;
        }

        let key = (state.resources, state.robots, state.minutes_left());

        if let Some(result) = self.memo.get(&key) {
            return Some(*result);
        }

        let mut running_max = 0;
        state.foreach_next_state(blueprint, |state| {
            if let Some(value) = self.evaluate_from(blueprint, state) {
                running_max = running_max.max(value);
            }
        });

        self.memo.insert(key, running_max);

        Some(running_max)
    }

    fn evaluate(&mut self, blueprint: &Blueprint, target: i32) -> i64 {
        assert!(target >= self.evaluated_for_time); // Must evaluate in order of increasing scope
        self.evaluated_for_time = target;

        self.evaluate_from(blueprint, &State::initial_state(target))
            .unwrap()
    }
}

fn parse_scenario(s: String) -> Result<Vec<Blueprint>> {
    let re = Regex::new(
        r"Blueprint [0-9]+: Each ore robot costs (?P<ore_ore>[0-9]+) ore. Each clay robot costs (?P<clay_ore>[0-9]+) ore. Each obsidian robot costs (?P<obs_ore>[0-9]+) ore and (?P<obs_clay>[0-9]+) clay. Each geode robot costs (?P<geo_ore>[0-9]+) ore and (?P<geo_obs>[0-9]+) obsidian.",
    )?;

    let lines: Vec<String> = s.trim().split('\n').map(|line| line.to_string()).collect();

    Ok(lines
        .iter()
        .map(|line| {
            let captures = re.captures(line.trim()).unwrap();
            [
                [captures["ore_ore"].parse().unwrap(), 0, 0, 0],
                [captures["clay_ore"].parse().unwrap(), 0, 0, 0],
                [
                    captures["obs_ore"].parse().unwrap(),
                    captures["obs_clay"].parse().unwrap(),
                    0,
                    0,
                ],
                [
                    captures["geo_ore"].parse().unwrap(),
                    0,
                    captures["geo_obs"].parse().unwrap(),
                    0,
                ],
            ]
        })
        .collect())
}

fn main() -> Result<()> {
    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s)?;

    let blueprints: Vec<Blueprint> = parse_scenario(s)?;

    let mut sum_of_qualities = 0;
    let mut product_of_geodes = 1;
    let mut total_iterations = 0;

    for (i, blueprint) in blueprints.iter().enumerate() {
        let mut memo = Memo::new();
        let id = (i + 1) as i64;
        let geodes_24 = memo.evaluate(blueprint, 24);
        let quality = id * geodes_24;
        sum_of_qualities += quality;

        println!(
            "Blueprint {} (after {} iterations): {} after 24",
            id, memo.iter, geodes_24,
        );

        if i < 3 {
            let geodes_32 = memo.evaluate(blueprint, 32);
            println!(
                "Blueprint {} (after {} iterations): {} after 32",
                id, memo.iter, geodes_32,
            );
            product_of_geodes *= geodes_32;
        }

        total_iterations += memo.iter;
    }

    println!("Total iterations used: {}", total_iterations);

    println!();
    println!("Answer to part A: {}", sum_of_qualities);
    println!("Answer to part B: {}", product_of_geodes);

    Ok(())
}
