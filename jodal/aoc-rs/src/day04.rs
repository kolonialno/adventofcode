use lazy_static::lazy_static;
use regex::Regex;
use serde::Deserialize;
use std::collections::HashMap;
use std::str::Lines;
use validator::{Validate, ValidationError};

pub fn solve_a(passports: &Vec<Passport>) -> usize {
    passports.into_iter().filter(|p| p.is_valid_a()).count()
}

pub fn solve_b(passports: &Vec<Passport>) -> usize {
    passports.into_iter().filter(|p| p.is_valid_b()).count()
}

lazy_static! {
    static ref HEIGHT: Regex = Regex::new(r"^(\d{3}cm|\d{2}in)$").unwrap();
    static ref HAIR_COLOR: Regex = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
    static ref EYE_COLOR: Regex = Regex::new(r"^(amb|blu|brn|gry|grn|hzl|oth)$").unwrap();
    static ref PID: Regex = Regex::new(r"^\d{9}$").unwrap();
}

#[derive(Debug, Validate, Deserialize)]
pub struct Passport {
    #[validate(range(min = 1920, max = 2002))]
    byr: Option<u32>,
    #[validate(range(min = 2010, max = 2020))]
    iyr: Option<u32>,
    #[validate(range(min = 2020, max = 2030))]
    eyr: Option<u32>,
    #[validate(regex = "HEIGHT", custom = "validate_height")]
    hgt: Option<String>,
    #[validate(regex = "HAIR_COLOR")]
    hcl: Option<String>,
    #[validate(regex = "EYE_COLOR")]
    ecl: Option<String>,
    #[validate(regex = "PID")]
    pid: Option<String>,
    cid: Option<String>,
}

fn validate_height(height: &str) -> Result<(), ValidationError> {
    if height.ends_with("cm") {
        match &height[0..3].parse::<u32>() {
            Ok(number) => {
                if (150..=193).contains(number) {
                    return Ok(());
                } else {
                    return Err(ValidationError::new("bad height"));
                }
            }
            Err(_) => {
                return Err(ValidationError::new("bad height"));
            }
        }
    } else {
        match &height[0..2].parse::<u32>() {
            Ok(number) => {
                if (59..=76).contains(number) {
                    return Ok(());
                } else {
                    return Err(ValidationError::new("bad height"));
                }
            }
            Err(_) => {
                return Err(ValidationError::new("bad height"));
            }
        }
    }
}

impl Passport {
    pub fn from_lines(lines: Lines) -> Vec<Passport> {
        lines
            .collect::<Vec<&str>>()
            .split(|line| line.is_empty())
            .map(|passport_lines| passport_lines.join(" "))
            .map(|passport_line| Passport::from_line(&passport_line))
            .collect()
    }

    fn from_line(line: &str) -> Passport {
        let mut map = HashMap::new();
        for parts in line
            .split(' ')
            .map(|pair| pair.split(':').collect::<Vec<&str>>())
        {
            if parts.len() == 2 {
                map.insert(parts[0].to_owned(), parts[1].to_owned());
            }
        }
        Passport {
            byr: map.get("byr").map(|s| s.parse::<u32>().ok()).flatten(),
            iyr: map.get("iyr").map(|s| s.parse::<u32>().ok()).flatten(),
            eyr: map.get("eyr").map(|s| s.parse::<u32>().ok()).flatten(),
            hgt: map.get("hgt").map(|s| s.to_owned()),
            hcl: map.get("hcl").map(|s| s.to_owned()),
            ecl: map.get("ecl").map(|s| s.to_owned()),
            pid: map.get("pid").map(|s| s.to_owned()),
            cid: map.get("cid").map(|s| s.to_owned()),
        }
    }

    fn is_valid_a(&self) -> bool {
        self.byr.is_some()
            && self.iyr.is_some()
            && self.eyr.is_some()
            && self.hgt.is_some()
            && self.hcl.is_some()
            && self.ecl.is_some()
            && self.pid.is_some()
    }

    fn is_valid_b(&self) -> bool {
        self.is_valid_a() && self.validate().is_ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_a() {
        let input = String::from(
            r"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
",
        );
        let passports = Passport::from_lines(input.lines());
        assert_eq!(solve_a(&passports), 2);
    }

    #[test]
    fn example_b_invalid() {
        let input = String::from(
            r"eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
",
        );
        let passports = Passport::from_lines(input.lines());
        assert_eq!(solve_b(&passports), 0);
    }

    #[test]
    fn example_b_valid() {
        let input = String::from(
            r"pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
",
        );
        let passports = Passport::from_lines(input.lines());
        assert_eq!(solve_b(&passports), 4);
    }

    #[test]
    fn test_validate_height() {
        assert!(validate_height("149cm").is_err());
        assert!(validate_height("150cm").is_ok());
        assert!(validate_height("193cm").is_ok());
        assert!(validate_height("194cm").is_err());
    }
}
