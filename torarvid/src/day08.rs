use std::str::FromStr;

use crate::util::file_by_lines;

pub fn run() {
    let signals = file_by_lines("day08.txt");
    let mut count = 0;
    for line in &signals {
        let outputs = line.split(" | ").nth(1).unwrap();
        let parts = outputs.split(" ").collect::<Vec<&str>>();
        let length_3 = parts.iter().filter(|x| x.len() == 3).count();
        let length_2 = parts.iter().filter(|x| x.len() == 2).count();
        let length_4 = parts.iter().filter(|x| x.len() == 4).count();
        let length_7 = parts.iter().filter(|x| x.len() == 7).count();
        count += length_3 + length_2 + length_4 + length_7;
    }

    println!("Part 1: {}", count);

    let mut sum = 0;
    for line in &signals {
        let seven_segment_display = Segment::from_str(line).unwrap();
        sum += seven_segment_display.parse();
    }

    println!("Part 2: {}", sum);
}

struct Segment {
    raw: String,
}

impl Segment {
    fn contains(a: &str, b: &str) -> bool {
        a.chars().all(|c| b.contains(c))
    }

    fn eq(a: &str, b: &str) -> bool {
        Segment::contains(a, b) && Segment::contains(b, a)
    }

    fn detect_1(examples: Vec<&str>) -> String {
        examples.iter().find(|x| x.len() == 2).unwrap().to_string()
    }

    fn detect_7(examples: Vec<&str>) -> String {
        examples.iter().find(|x| x.len() == 3).unwrap().to_string()
    }

    fn detect_4(examples: Vec<&str>) -> String {
        examples.iter().find(|x| x.len() == 4).unwrap().to_string()
    }

    fn detect_8(examples: Vec<&str>) -> String {
        examples.iter().find(|x| x.len() == 7).unwrap().to_string()
    }

    fn detect_0_6_and_9(
        examples: Vec<&str>,
        digit_4: &str,
        digit_7: &str,
    ) -> (String, String, String) {
        let len_6 = examples
            .iter()
            .filter(|x| x.len() == 6)
            .collect::<Vec<&&str>>();

        let zero = len_6
            .iter()
            .filter(|x| Segment::contains(digit_7, x) && !Segment::contains(digit_4, x))
            .next()
            .unwrap()
            .to_string();

        let six = len_6
            .iter()
            .filter(|x| !Segment::contains(digit_7, x))
            .next()
            .unwrap()
            .to_string();

        let nine = len_6
            .iter()
            .filter(|x| Segment::contains(digit_7, x) && Segment::contains(digit_4, x))
            .next()
            .unwrap()
            .to_string();

        (zero, six, nine)
    }

    fn detect_2_3_and_5(
        examples: Vec<&str>,
        digit_6: &str,
        digit_7: &str,
    ) -> (String, String, String) {
        let len_5 = examples
            .iter()
            .filter(|x| x.len() == 5)
            .collect::<Vec<&&str>>();

        let two = len_5
            .iter()
            .filter(|x| !Segment::contains(x, digit_6) && !Segment::contains(digit_7, x))
            .next()
            .unwrap()
            .to_string();

        let three = len_5
            .iter()
            .filter(|x| Segment::contains(digit_7, x))
            .next()
            .unwrap()
            .to_string();

        let five = len_5
            .iter()
            .filter(|x| Segment::contains(x, digit_6))
            .next()
            .unwrap()
            .to_string();

        (two, three, five)
    }

    fn parse(&self) -> u32 {
        let mut parts = self.raw.split(" | ");
        let examples = parts.next().unwrap().split(" ").collect::<Vec<&str>>();
        let outputs = parts.next().unwrap().split(" ").collect::<Vec<&str>>();
        let digit_1 = Segment::detect_1(examples.clone());
        let digit_7 = Segment::detect_7(examples.clone());
        let digit_4 = Segment::detect_4(examples.clone());
        let digit_8 = Segment::detect_8(examples.clone());
        let (zero, six, nine) = Segment::detect_0_6_and_9(examples.clone(), &digit_4, &digit_7);
        let (two, three, five) = Segment::detect_2_3_and_5(examples.clone(), &six, &digit_7);
        let mut number = 0;
        outputs.iter().for_each(|output| {
            number *= 10;
            if Segment::eq(output, &digit_1) {
                number += 1;
            } else if Segment::eq(output, &digit_4) {
                number += 4;
            } else if Segment::eq(output, &digit_7) {
                number += 7;
            } else if Segment::eq(output, &digit_8) {
                number += 8;
            } else if Segment::eq(output, &zero) {
                number += 0;
            } else if Segment::eq(output, &six) {
                number += 6;
            } else if Segment::eq(output, &nine) {
                number += 9;
            } else if Segment::eq(output, &two) {
                number += 2;
            } else if Segment::eq(output, &three) {
                number += 3;
            } else if Segment::eq(output, &five) {
                number += 5;
            } else {
                panic!("Unknown output: {}", output);
            }
        });
        number
    }
}

impl FromStr for Segment {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Segment { raw: s.to_string() })
    }
}
