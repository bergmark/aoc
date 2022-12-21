use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s08.txt"), 5);
    assert_eq!(a("txt/e08.txt"), 1487);
    assert_eq!(b("txt/s08.txt"), 8);
    assert_eq!(b("txt/e08.txt"), 1607);
}

fn a(filename: &str) -> i32 {
    let program: Vec<Ins> = read_parsed(filename).collect();
    let mut visited: Vec<bool> = program.iter().map(|_| false).collect();
    let mut acc: i32 = 0;
    let mut pc: usize = 0;
    loop {
        if visited[pc] {
            return acc;
        }
        visited[pc] = true;
        let cmd = program[pc];
        use Ins::*;
        match cmd {
            Nop(_) => pc += 1,
            Acc(i) => {
                acc += i;
                pc += 1
            }
            Jmp(i) => pc = (pc as i32 + i) as usize,
        }
    }
}

fn b(filename: &str) -> i32 {
    use Ins::*;
    let program: Vec<Ins> = read_parsed(filename).collect();
    let program_len = program.len();
    let mut attempts = vec![false; program_len];
    let mut visited: Vec<bool> = vec![false; program_len];
    'outer: loop {
        visited.fill(false);
        let mut swapped = false;
        let mut acc: i32 = 0;
        let mut pc: usize = 0;
        loop {
            if pc == program_len {
                return acc;
            }
            if visited[pc] {
                continue 'outer;
            }
            visited[pc] = true;
            let cmd = if !swapped && !attempts[pc] {
                swapped = true;
                attempts[pc] = true;
                match program[pc] {
                    Nop(i) => Jmp(i),
                    Acc(i) => {
                        swapped = false;
                        Acc(i)
                    }
                    Jmp(i) => Nop(i),
                }
            } else {
                program[pc]
            };
            match cmd {
                Nop(_) => pc += 1,
                Acc(i) => {
                    acc += i;
                    pc += 1
                }
                Jmp(i) => pc = (pc as i32 + i) as usize,
            }
        }
    }
}

#[derive(Clone, Copy)]
enum Ins {
    Nop(i32),
    Acc(i32),
    Jmp(i32),
}

impl FromStr for Ins {
    type Err = ();
    fn from_str(s: &str) -> Result<Ins, ()> {
        let (cmd, val) = split2(s, " ")?;
        let val = val.parse().unwrap();
        use Ins::*;
        let cmd = match cmd {
            "nop" => Nop(val),
            "acc" => Acc(val),
            "jmp" => Jmp(val),
            _ => unreachable!(),
        };
        Ok(cmd)
    }
}
