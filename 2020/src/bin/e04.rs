use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    tests();
    assert_eq!(a("txt/s04.txt", false), 2);
    assert_eq!(a("txt/e04.txt", false), 213);
    assert_eq!(a("txt/s04.txt", true), 2);
    assert_eq!(a("txt/e04.txt", true), 147);
}

fn a(s: &str, field_validation: bool) -> usize {
    let mut passports: Vec<Passport> = vec![];
    let mut current_passport: Passport = Passport::default();
    for line in read_parsed(s) {
        match line {
            Line::Blank => {
                if current_passport.len() >= 1 {
                    passports.push(current_passport);
                }
                current_passport = Passport::default();
            }
            Line::Pass { entries } => {
                for (k, v) in entries {
                    current_passport.insert(k, v);
                }
            }
        }
    }
    if current_passport.len() >= 1 {
        passports.push(current_passport);
    }

    passports
        .into_iter()
        .filter(|c| validate(c, field_validation))
        .count()
}

fn validate(p: &Passport, field_validation: bool) -> bool {
    const REQUIRED: [Header; 7] = [
        Header::Byr,
        Header::Iyr,
        Header::Eyr,
        Header::Hgt,
        Header::Hcl,
        Header::Ecl,
        Header::Pid,
    ];
    if p.len() < 6 {
        return false;
    }
    for r in REQUIRED {
        match p.get(r) {
            None => return false,
            Some(v) if field_validation && !valid_field(r, v) => return false,
            _ => (),
        }
    }
    true
}

fn valid_field(p: Header, v: &str) -> bool {
    fn within(s: &str, min: usize, max: usize) -> bool {
        s.parse().map_or(false, |v: usize| v >= min && v <= max)
    }

    use Header::*;
    match p {
        Byr => within(v, 1920, 2002),
        Iyr => within(v, 2010, 2020),
        Eyr => within(v, 2020, 2030),
        Hgt => {
            let (n, m) = take_while(v, |c| c.is_digit(10));
            match &*m {
                "in" => within(&n, 59, 76),
                "cm" => within(&n, 150, 193),
                _ => false,
            }
        }
        Hcl => {
            let (a, b) = take_while(v, |c| c == '#');
            a == "#" && b.chars().all(|c| c.is_digit(16))
        }
        Ecl => {
            const VS: [&str; 7] = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];
            VS.contains(&v)
        }
        Pid => v.len() == 9 && v.chars().all(|c| c.is_digit(10)),
        Cid => true,
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum Header {
    Byr,
    Iyr,
    Eyr,
    Hgt,
    Hcl,
    Ecl,
    Pid,
    Cid,
}

impl FromStr for Header {
    type Err = ();
    fn from_str(s: &str) -> Result<Header, ()> {
        match s {
            "byr" => Ok(Header::Byr),
            "iyr" => Ok(Header::Iyr),
            "eyr" => Ok(Header::Eyr),
            "hgt" => Ok(Header::Hgt),
            "hcl" => Ok(Header::Hcl),
            "ecl" => Ok(Header::Ecl),
            "pid" => Ok(Header::Pid),
            "cid" => Ok(Header::Cid),
            _ => Err(()),
        }
    }
}

pub enum Line {
    Blank,
    Pass { entries: Vec<(Header, String)> },
}

impl FromStr for Line {
    type Err = ();
    fn from_str(s: &str) -> Result<Line, ()> {
        if s.is_empty() {
            return Ok(Line::Blank);
        }
        let mut entries = vec![];
        for pair in s.split(' ') {
            let (k, v) = split2(pair, ":")?;
            entries.push((Header::from_str(k)?, v.to_owned()));
        }
        Ok(Line::Pass { entries })
    }
}

#[derive(PartialEq, Eq, Debug, Default)]
struct Passport(HashMap<Header, String>);

impl Passport {
    fn len(&self) -> usize {
        self.0.len()
    }
    fn insert(&mut self, k: Header, v: String) {
        self.0.insert(k, v);
    }
    fn get(&self, k: Header) -> Option<&String> {
        self.0.get(&k)
    }
}

fn tests() {
    use Header::*;
    assert!(valid_field(Byr, "2002"));
    assert!(!valid_field(Byr, "2003"));
    assert!(valid_field(Hgt, "60in"));
    assert!(valid_field(Hgt, "190cm"));
    assert!(!valid_field(Hgt, "190in"));
    assert!(!valid_field(Hgt, "190"));
    assert!(valid_field(Hcl, "#123abc"));
    assert!(!valid_field(Hcl, "#123abz"));
    assert!(!valid_field(Hcl, "123abc"));
    assert!(valid_field(Ecl, "brn"));
    assert!(!valid_field(Ecl, "wat"));
    assert!(valid_field(Pid, "000000001"));
    assert!(!valid_field(Pid, "0123456789"));
}
