use std::fs;

fn main() {
    let prog = fs::read_to_string("input.txt").expect("Could not read file").split(",").map(|s| s.parse::<i32>().unwrap()).collect::<Vec<i32>>();
    let answer = eval(State { pc: 0, memory: prog });
    print!("{:?}", answer);
}

#[derive(Debug)]
pub enum Instr {
    Add(Arg, Arg, Position),
    Mul(Arg, Arg, Position),
    Read(Position),
    Write(Arg),
    Halt
}

#[derive(Debug)]
pub enum Op {
    Add,
    Mul,
    Read,
    Write,
    Halt
}

#[derive(Debug)]
pub struct Position(usize);

#[derive(Debug)]
pub struct Immediate(i32);

#[derive(Debug)]
pub enum Arg {
    Pos(Position),
    Imm(Immediate)
}

#[derive(Debug)]
pub enum Mode {
    Position,
    Immediate
}

fn pos_in(v: i32, i: usize) -> u32 {
    v.to_string().chars().rev().nth(i).unwrap_or('0').to_digit(10).unwrap()
}

fn to_mode(u: u32) -> Mode {
    match u {
        0 => Mode::Position,
        1 => Mode::Immediate,
        _ => panic!("Invalid mode {}", u)
    }
}

fn to_op(a: u32, b: u32) -> Op {
    match (a, b) {
        (0, 1) => Op::Add,
        (0, 2) => Op::Mul,
        (0, 3) => Op::Read,
        (0, 4) => Op::Write,
        (9, 9) => Op::Halt,
        _ => panic!("Invalid OpCode {:?}", (a, b))
    }
}

fn to_arg(mode: Mode, value: i32) -> Arg {
    match mode {
        Mode::Immediate => Arg::Imm(Immediate(value)),
        Mode::Position  => Arg::Pos(Position(value as usize))
    }
}

pub struct State {
    pc: usize,
    memory: Vec<i32>
}

impl State {
    fn read(&mut self) -> i32 {
        self.pc += 1;
        self.memory[self.pc - 1]
    }
    fn save(&mut self, p: Position, v: i32) -> () {
        self.memory[p.0] = v
    }
    fn read_arg(&self, a: Arg) -> i32 {
        match a {
            Arg::Pos(Position(p)) => self.memory[p],
            Arg::Imm(Immediate(i)) => i
        }
    }
}

fn parse_instruction(s: &mut State) -> Instr {
    let instr = s.read();
    let op = to_op(pos_in(instr, 1), pos_in(instr, 0));
    let mode_1 = to_mode(pos_in(instr, 2));
    let mode_2 = to_mode(pos_in(instr, 3));
    match op {
        Op::Add =>
            Instr::Add(
                to_arg(mode_1, s.read()),
                to_arg(mode_2, s.read()),
                Position(s.read() as usize)
            ),
        Op::Mul =>
            Instr::Mul(
                to_arg(mode_1, s.read()),
                to_arg(mode_2, s.read()),
                Position(s.read() as usize)
            ),
        Op::Read =>
            Instr::Read(
                Position(s.read() as usize)
            ),
        Op::Write =>
            Instr::Write(
                to_arg(mode_1, s.read())
            ),
        Op::Halt =>
            Instr::Halt
    }
}

fn eval(mut s: State) {
    let instr = parse_instruction(&mut s);
    match instr {
        Instr::Add(a, b, res) => {
            s.save(res, s.read_arg(a) + s.read_arg(b))
        },
        Instr::Mul(a, b, res) => {
            s.save(res, s.read_arg(a) * s.read_arg(b))
        },
        Instr::Read(res) => {
            let mut buffer = String::new();
            println!("INPUT:");
            std::io::stdin().read_line(&mut buffer).expect("Failed");
            let x = buffer.trim().parse::<i32>().unwrap();
            s.save(res, x)
        },
        Instr::Write(a) => {
            println!("TEST WRITE {}", s.read_arg(a));
        },
        Instr::Halt => {
            println!("HALT");
            return
        }
    }
    eval(s);
}
