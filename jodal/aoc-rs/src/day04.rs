use std::collections::HashMap;

pub struct Passport {
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
    pub fn from_lines(lines: Vec<String>) -> Vec<Passport> {
        lines
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
            byr: map.get("byr").map(|s| s.to_owned()),
            iyr: map.get("iyr").map(|s| s.to_owned()),
            eyr: map.get("eyr").map(|s| s.to_owned()),
            hgt: map.get("hgt").map(|s| s.to_owned()),
            hcl: map.get("hcl").map(|s| s.to_owned()),
            ecl: map.get("ecl").map(|s| s.to_owned()),
            pid: map.get("pid").map(|s| s.to_owned()),
            cid: map.get("cid").map(|s| s.to_owned()),
        }
    }

    pub fn is_valid(&self) -> bool {
        self.byr.is_some()
            && self.iyr.is_some()
            && self.eyr.is_some()
            && self.hgt.is_some()
            && self.hcl.is_some()
            && self.ecl.is_some()
            && self.pid.is_some()
    }
}

pub fn solve_a(passports: Vec<Passport>) -> usize {
    passports.into_iter().filter(|p| p.is_valid()).count()
}

pub fn solve_b() -> u32 {
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_a() {
        let passports = Passport::from_lines(
            r###"
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
"###
            .to_string()
            .trim()
            .lines()
            .map(|s| s.to_string())
            .collect(),
        );
        assert_eq!(solve_a(passports), 2);
    }

    #[test]
    fn example_b() {
        assert_eq!(solve_b(), 0);
    }
}
