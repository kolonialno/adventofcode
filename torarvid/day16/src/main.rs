use std::collections::HashMap;
use std::fs;
use std::ops::Range;

fn main() {
    let content = fs::read_to_string("input.txt").expect("Failed to read input.txt");
    println!("Part1: {}", solve_part1(&content));
    println!("Part2: {}", solve_part2(&content));
}

#[derive(Debug, Clone)]
struct Rule {
    name: String,
    range1: Range<usize>,
    range2: Range<usize>,
}

impl Rule {
    fn parse(input: &str) -> Rule {
        let mut parts = input.split(": ");
        let name = String::from(parts.next().unwrap());
        let line_ranges = parts.next().unwrap().split(" or ");
        let mut line_rranges = Vec::new();
        for range in line_ranges {
            let mut numbers = range.split("-");
            let min = numbers.next().unwrap().parse::<usize>().unwrap();
            let max = numbers.next().unwrap().parse::<usize>().unwrap();
            line_rranges.push(min..max + 1);
        }
        Rule {
            name,
            range1: line_rranges[0].clone(),
            range2: line_rranges[1].clone(),
        }
    }
}

impl PartialEq for Rule {
    fn eq(&self, rhs: &Rule) -> bool {
        self.name == rhs.name
    }
}

#[derive(Clone, Debug)]
struct Rules {
    rules: Vec<Rule>,
}

impl Rules {
    fn valid(&self, ticket: &str) -> (bool, usize) {
        for part in ticket.split(",") {
            let num = part.parse::<usize>().unwrap();
            match self
                .rules
                .iter()
                .find(|r| r.range1.contains(&num) || r.range2.contains(&num))
            {
                Some(_) => {}
                None => return (false, num),
            }
        }
        return (true, 0);
    }
}

fn solve_part1(input: &str) -> usize {
    let parts = input.split("\n\n").collect::<Vec<&str>>();
    let rule_collection = parts[0].lines().map(Rule::parse).collect::<Vec<Rule>>();
    let rules = Rules {
        rules: rule_collection,
    };
    let mut sum = 0;
    for ticket in parts[2].lines().skip(1) {
        let partial_sum: usize = rules.valid(ticket).1;
        sum += partial_sum;
    }
    sum
}

fn solve_part2(input: &str) -> usize {
    let parts = input.split("\n\n").collect::<Vec<&str>>();
    let rule_collection = parts[0].lines().map(Rule::parse).collect::<Vec<Rule>>();
    let rules = Rules {
        rules: rule_collection,
    };
    let valid_tickets = parts[2]
        .lines()
        .skip(1)
        .filter(|t| rules.valid(t).0)
        .map(String::from)
        .collect::<Vec<String>>();
    let mut new_rules = rules.clone();
    let mut mapped_columns: HashMap<usize, Rule> = HashMap::new();
    loop {
        let result = map_single_column(&mut new_rules, valid_tickets.iter(), &mapped_columns);
        if let Some(index_to_map) = result.0 {
            mapped_columns.insert(index_to_map.0, index_to_map.1);
        } else {
            break;
        }
        new_rules = result.1;
    }
    let mut product: usize = 1;
    let my_ticket = parts[1]
        .lines()
        .nth(1)
        .unwrap()
        .split(",")
        .map(|num| num.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    for (k, v) in mapped_columns {
        if v.name.contains("departure") {
            product *= my_ticket[k];
        }
    }
    product
}

fn map_single_column<'a, Iter>(
    rules: &mut Rules,
    valid_tickets: Iter,
    mapped_columns: &HashMap<usize, Rule>,
) -> (Option<(usize, Rule)>, Rules)
where
    Iter: Iterator<Item = &'a String>,
{
    let mut ticket_nums = Vec::new();
    let mut rules_that_fit = Vec::new();
    for ticket in valid_tickets {
        let mut numbers = Vec::new();
        for (i, part) in ticket.split(",").enumerate() {
            if i <= rules_that_fit.len() {
                rules_that_fit.push(rules.clone());
            }
            let num = part.parse::<usize>().unwrap();
            if mapped_columns.contains_key(&num) {
                continue;
            }
            if let Some(ii) = rules_that_fit[i]
                .rules
                .iter()
                .position(|r| !r.range1.contains(&num) && !r.range2.contains(&num))
            {
                rules_that_fit[i].rules.remove(ii);
            }
            numbers.push(num);
        }
        ticket_nums.push(numbers);
    }
    for (i, rule_set) in rules_that_fit.iter().enumerate() {
        if rule_set.rules.len() == 1 {
            let rule = rule_set.rules.first().unwrap();
            rules.rules.retain(|x| x != rule);
            return (
                Some((i, rule.clone())),
                Rules {
                    rules: rules.rules.clone(),
                },
            );
        }
    }
    (None, rules.clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let test_data = "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12";
        assert_eq!(solve_part1(test_data), 71);
    }
}
