use color_eyre::eyre::Result;
use itertools::{process_results, Itertools};
use std::str::FromStr;

#[derive(PartialEq, Debug)]
struct Section {
    start: u32,
    end: u32,
}

impl Section {
    fn contains(&self, other: &Section) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    // Cases we want to account for
    // A <------>
    // B    <------>
    //
    // A    <------>
    // B <------>
    fn partial_overlap(&self, other: &Section) -> bool {
        (self.start <= other.start && self.end >= other.start)
            || (self.start >= other.start && self.start <= other.end)
    }
}

impl TryFrom<&str> for Section {
    type Error = color_eyre::Report;

    fn try_from(range: &str) -> Result<Self, Self::Error> {
        let Some((start, end)) = range.split_once('-') else {
            return Err(color_eyre::eyre::eyre!("expected <start>-<end>EOF, got {range:?}"));
        };

        Ok(Section {
            start: start.parse::<u32>()?,
            end: end.parse::<u32>()?,
        })
    }
}

struct Sections {
    a: Section,
    b: Section,
}

impl Sections {
    fn contains(&self) -> bool {
        self.a.contains(&self.b) || self.b.contains(&self.a)
    }

    // Is a superset of containing, so we want to account for containment too
    fn overlap(&self) -> bool {
        self.contains() || self.a.partial_overlap(&self.b)
    }
}

impl FromStr for Sections {
    type Err = color_eyre::Report;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let Some((a, b)) = input.split_once(',') else {
            return Err(color_eyre::eyre::eyre!("expected <section a>-<section b>EOF, got {input:?}"));
        };
        Ok(Sections {
            a: a.try_into()?,
            b: b.try_into()?,
        })
    }
}

fn main() -> color_eyre::Result<()> {
    let input = include_str!("../input.txt");
    let sections = input
        .lines()
        .map(Sections::from_str)
        .filter_ok(Sections::contains);
    let contains_count = process_results(sections, |s| s.count())?;
    println!("{contains_count} sections that contain each other");

    let sections = input
        .lines()
        .map(Sections::from_str)
        .filter_ok(Sections::overlap);
    let overlapping_count = process_results(sections, |s| s.count())?;
    println!("{overlapping_count} sections that overlap");

    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{Section, Sections};
    use itertools::{process_results, Itertools};
    use std::str::FromStr;

    #[test]
    fn containment() {
        assert!(Section { start: 1, end: 2 }.contains(&Section { start: 1, end: 1 }));
        assert!(!Section { start: 1, end: 1 }.contains(&Section { start: 1, end: 2 }));
    }

    #[test]
    fn partial_overlap() {
        assert!(Section { start: 1, end: 2 }.partial_overlap(&Section { start: 2, end: 3 }));
        assert!(Section { start: 2, end: 3 }.partial_overlap(&Section { start: 1, end: 2 }));
        assert!(!Section { start: 1, end: 2 }.partial_overlap(&Section { start: 3, end: 4 }));
    }

    #[test]
    fn parsing_section() {
        let result = "10-200".try_into();
        assert!(result.is_ok());
        assert_eq!(
            Section {
                start: 10,
                end: 200
            },
            result.unwrap()
        );

        let result: Result<Section, _> = "122".try_into();
        assert!(result.is_err());
    }

    #[test]
    fn validate_sample_part1() {
        let input = include_str!("../sample.txt");

        let sections = input
            .lines()
            .map(Sections::from_str)
            .filter_ok(Sections::contains);
        let count = process_results(sections, |s| s.count()).unwrap();
        assert_eq!(count, 2);
    }

    #[test]
    fn validate_sample_part2() {
        let input = include_str!("../sample.txt");

        let sections = input
            .lines()
            .map(Sections::from_str)
            .filter_ok(Sections::overlap);
        let count = process_results(sections, |s| s.count()).unwrap();
        assert_eq!(count, 4);
    }
}
