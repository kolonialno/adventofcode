use regex::Regex;

#[derive(Debug, Clone, Copy)]
struct Section {
    start: i32,
    end: i32,
}

impl Section {
    fn any_overlap(self, other: Section) -> bool {
        if self.start > other.end {
            return false;
        }
        if self.end < other.start {
            return false;
        }
        true
    }
    fn full_overlap(self, other: Section) -> bool {
        if self.start >= other.start && self.end <= other.end {
            return true;
        }
        if self.start <= other.start && self.end >= other.end {
            return true;
        }
        false
    }
}

fn main() {
    let input = include_str!("input.txt");
    let re = Regex::new(r"^(\d+)-(\d+),(\d+)-(\d+)$").unwrap();
    let pairs: Vec<[Section; 2]> = input
        .lines()
        .map(|line| {
            let cap = re.captures(line).unwrap();
            [
                Section {
                    start: cap[1].parse::<i32>().unwrap(),
                    end: cap[2].parse::<i32>().unwrap(),
                },
                Section {
                    start: cap[3].parse::<i32>().unwrap(),
                    end: cap[4].parse::<i32>().unwrap(),
                },
            ]
        })
        .collect();

    dbg!(pairs
        .iter()
        .map(|p| p[0].full_overlap(p[1]) as i32)
        .sum::<i32>());

    dbg!(pairs
        .iter()
        .map(|p| p[0].any_overlap(p[1]) as i32)
        .sum::<i32>());
}
