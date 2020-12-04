use std::format;
use std::fs;
use std::str::Lines;

fn main() {
    let filename = "input.txt";
    let contents =
        fs::read_to_string(filename).expect(&format!("Failed to read file {}", filename));
    println!("Part1: {}", solve_part1(contents.lines()));
    println!("Part2: {}", solve_part2(contents.lines()));
}

#[derive(Default)]
struct Passport {
    byr: Option<String>,
    iyr: Option<String>,
    eyr: Option<String>,
    hgt: Option<String>,
    hcl: Option<String>,
    ecl: Option<String>,
    pid: Option<String>,
    cid: Option<String>,
}

impl Passport {
    fn valid(&self) -> bool {
        self.byr.is_some()
            && self.iyr.is_some()
            && self.eyr.is_some()
            && self.hgt.is_some()
            && self.hcl.is_some()
            && self.ecl.is_some()
            && self.pid.is_some()
    }
}

fn solve_part1(input: Lines) -> i32 {
    let mut valid_passports = 0;
    let mut current_passport: Passport = Default::default();
    for line in input {
        if line.chars().count() == 0 {
            if current_passport.valid() {
                valid_passports += 1;
            }
            current_passport = Default::default();
            continue;
        }
        let sections = line.split(" ");
        for section in sections {
            let parts: Vec<&str> = section.split(":").collect();
            let (key, val) = (parts[0], parts[1]);
            match key {
                "byr" => current_passport.byr = Some(String::from(val)),
                "iyr" => current_passport.iyr = Some(String::from(val)),
                "eyr" => current_passport.eyr = Some(String::from(val)),
                "hgt" => current_passport.hgt = Some(String::from(val)),
                "hcl" => current_passport.hcl = Some(String::from(val)),
                "ecl" => current_passport.ecl = Some(String::from(val)),
                "pid" => current_passport.pid = Some(String::from(val)),
                "cid" => current_passport.cid = Some(String::from(val)),
                _ => continue,
            }
        }
    }
    if current_passport.valid() {
        valid_passports += 1;
    }
    valid_passports
}

#[derive(Default)]
struct StrictPassport {
    byr: Option<i32>,
    iyr: Option<i32>,
    eyr: Option<i32>,
    hgt: Option<i32>,
    hcl: Option<String>,
    ecl: Option<String>,
    pid: Option<i32>,
    cid: Option<String>,
}

impl StrictPassport {
    fn valid(&self) -> bool {
        self.byr.is_some()
            && self.iyr.is_some()
            && self.eyr.is_some()
            && self.hgt.is_some()
            && self.hcl.is_some()
            && self.ecl.is_some()
            && self.pid.is_some()
    }
}

fn valid_num(input: &str, num_digits: usize, min: i32, max: i32) -> Option<i32> {
    input
        .trim()
        .parse::<i32>()
        .ok()
        .filter(|num| *num >= min && *num <= max)
        .filter(|num| num.to_string().chars().count() == num_digits)
}

fn valid_height(input: &str) -> Option<i32> {
    if input.ends_with("cm") {
        valid_num(&input[0..input.bytes().count() - 2], 3, 150, 193)
    } else if input.ends_with("in") {
        valid_num(&input[0..input.bytes().count() - 2], 2, 59, 76)
    } else {
        None
    }
}

fn valid_hair(input: &str) -> Option<String> {
    let chars: Vec<char> = input.chars().collect();
    fn valid_char(c: char) -> bool {
        c >= '0' && c <= '9' || c >= 'a' && c <= 'f'
    }
    if chars[0] == '#' && chars.len() == 7 && chars[1..].iter().all(|c| valid_char(*c)) {
        Some(String::from(input))
    } else {
        None
    }
}

fn valid_eye(input: &str) -> Option<String> {
    if ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(&input) {
        Some(String::from(input))
    } else {
        None
    }
}

fn valid_pid(input: &str) -> Option<i32> {
    let trimmed = input.trim();
    if trimmed.chars().count() != 9 {
        None
    } else {
        trimmed.parse::<i32>().ok()
    }
}

fn solve_part2(input: Lines) -> i32 {
    let mut valid_passports = 0;
    let mut current_passport: StrictPassport = Default::default();
    for line in input {
        if line.chars().count() == 0 {
            if current_passport.valid() {
                valid_passports += 1;
            }
            current_passport = Default::default();
            continue;
        }
        let sections = line.split(" ");
        for section in sections {
            let parts: Vec<&str> = section.split(":").collect();
            let (key, val) = (parts[0], parts[1]);
            match key {
                "byr" => current_passport.byr = valid_num(val, 4, 1920, 2002),
                "iyr" => current_passport.iyr = valid_num(val, 4, 2010, 2020),
                "eyr" => current_passport.eyr = valid_num(val, 4, 2020, 2030),
                "hgt" => current_passport.hgt = valid_height(val),
                "hcl" => current_passport.hcl = valid_hair(val),
                "ecl" => current_passport.ecl = valid_eye(val),
                "pid" => current_passport.pid = valid_pid(val),
                "cid" => current_passport.cid = Some(String::from(val)),
                _ => continue,
            }
        }
    }
    if current_passport.valid() {
        valid_passports += 1;
    }
    valid_passports
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let test_data = String::from(
            "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in",
        );

        assert_eq!(solve_part1(test_data.lines()), 2);

        let invalid_strict_data = String::from(
            "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007",
        );

        assert_eq!(solve_part2(invalid_strict_data.lines()), 0);

        let valid_strict_data = String::from(
            "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719",
        );

        assert_eq!(solve_part2(valid_strict_data.lines()), 4);
    }
}
