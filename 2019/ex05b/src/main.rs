use std::fs;

fn read_file(fp: &String) -> Vec<i32> {
    parse(&fs::read_to_string(fp).expect("Could not read file"))
}

fn parse(s: &String) -> Vec<i32> {
    s.split(',').map(|s| s.parse().unwrap()).collect()
}

fn main() {
    let mut s = State::new(read_file(&"input.txt".to_string()), vec![1]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&16434972));

    let ex0 = "3,9,8,9,10,9,4,9,99,-1,8".to_string();

    let mut s = State::new(parse(&ex0), vec![7]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&0));

    let mut s = State::new(parse(&ex0), vec![8]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&1));

    let mut s = State::new(parse(&ex0), vec![9]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&0));


    let ex1 = "3,9,7,9,10,9,4,9,99,-1,8".to_string();

    let mut s = State::new(parse(&ex1), vec![0]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&1));

    let mut s = State::new(parse(&ex1), vec![7]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&1));

    let mut s = State::new(parse(&ex1), vec![8]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&0));


    let ex2 = "3,3,1108,-1,8,3,4,3,99".to_string();

    let mut s = State::new(parse(&ex2), vec![7]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&0));

    let mut s = State::new(parse(&ex2), vec![8]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&1));

    let mut s = State::new(parse(&ex2), vec![9]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&0));


    let ex3 = "3,3,1107,-1,8,3,4,3,99".to_string();

    let mut s = State::new(parse(&ex3), vec![0]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&1));

    let mut s = State::new(parse(&ex3), vec![7]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&1));

    let mut s = State::new(parse(&ex3), vec![8]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&0));


    let jump0 = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9".to_string();

    let mut s = State::new(parse(&jump0), vec![0]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&0));

    let mut s = State::new(parse(&jump0), vec![3]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&1));

    let jump1 = "3,3,1105,-1,9,1101,0,0,12,4,12,99,1".to_string();

    let mut s = State::new(parse(&jump1), vec![0]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&0));

    let mut s = State::new(parse(&jump1), vec![3]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&1));


    println!("!!! START !!!");

    let mut s = State::new(read_file(&"input.txt".to_string()), vec![5]);
    eval(&mut s);
    assert_eq!(s.outputs.last(), Some(&16694270));

}

#[derive(Debug)]
pub enum Instr {
    Add(Arg, Arg, Pos),
    Mul(Arg, Arg, Pos),
    Read(Pos),
    Write(Arg),
    JumpIfTrue(Arg, Arg),
    JumpIfFalse(Arg, Arg),
    LessThan(Arg, Arg, Pos),
    Equal(Arg, Arg, Pos),
    Halt
}

#[derive(Debug)]
pub enum Op {
    Add,
    Mul,
    Read,
    Write,
    JumpIfTrue,
    JumpIfFalse,
    LessThan,
    Equal,
    Halt
}

impl Op {
    fn new(a: u32, b: u32) -> Op {
        match (a, b) {
            (0, 1) => Op::Add,
            (0, 2) => Op::Mul,
            (0, 3) => Op::Read,
            (0, 4) => Op::Write,
            (0, 5) => Op::JumpIfTrue,
            (0, 6) => Op::JumpIfFalse,
            (0, 7) => Op::LessThan,
            (0, 8) => Op::Equal,
            (9, 9) => Op::Halt,
            _ => panic!("Invalid OpCode {:?}", (a, b))
        }
    }
}

#[derive(Debug)]
pub struct Pos(usize);

#[derive(Debug)]
pub struct Imm(i32);

#[derive(Debug)]
pub enum Arg {
    Pos(Pos),
    Imm(Imm)
}

impl Arg {
    fn new(mode: Mode, value: i32) -> Arg {
        match mode {
            Mode::Imm => Arg::Imm(Imm(value)),
            Mode::Pos  => Arg::Pos(Pos(value as usize))
        }
    }
}

#[derive(Debug)]
pub enum Mode {
    Pos,
    Imm
}

impl Mode {
    fn new(u: u32) -> Mode {
        match u {
            0 => Mode::Pos,
            1 => Mode::Imm,
            _ => panic!("Invalid mode {}", u)
        }
    }
}

pub struct State {
    pc: Pos,
    memory: Vec<i32>,
    inputs: Vec<i32>,
    outputs: Vec<i32>
}
impl State {
    fn new(m: Vec<i32>, inputs: Vec<i32>) -> State {
        State {
            pc: Pos(0),
            memory: m,
            inputs: inputs,
            outputs: vec![]
        }
    }
}

impl State {
    fn next(&mut self) -> i32 {
        self.pc = Pos(self.pc.0 + 1);
        self.memory[self.pc.0 - 1]
    }
    fn next_arg(&mut self, mode: Mode) -> Arg {
        Arg::new(mode, self.next())
    }
    fn next_pos(&mut self) -> Pos {
        Pos(self.next() as usize)
    }
    fn save(&mut self, p: Pos, v: i32) {
        println!("memory[{:?}] = {}", p, v);
        self.memory[p.0] = v
    }
    fn read_arg(&self, a: Arg) -> i32 {
        match a {
            Arg::Pos(Pos(p)) => self.memory[p],
            Arg::Imm(Imm(i)) => i
        }
    }
    fn read_pos(&self, a: Arg) -> Pos {
        Pos(self.read_arg(a) as usize)
    }
}

fn op_and_mode(v: i32) -> Vec<u32> {
    let mut vec: Vec<_> = v.to_string().chars().map(|c| c.to_digit(10).unwrap()).collect();
    vec.reverse();
    vec.resize_with(5, || 0);
    vec
}

fn parse_instruction(s: &mut State) -> Instr {
    let instr = s.next();
    let om = op_and_mode(instr);
    let op = Op::new(om[1], om[0]);
    let mode_1 = Mode::new(om[2]);
    let mode_2 = Mode::new(om[3]);
    let _mode_3 = Mode::new(om[4]);
    match op {
        Op::Add =>
            Instr::Add(
                s.next_arg(mode_1),
                s.next_arg(mode_2),
                s.next_pos()
            ),
        Op::Mul =>
            Instr::Mul(
                s.next_arg(mode_1),
                s.next_arg(mode_2),
                s.next_pos()
            ),
        Op::Read =>
            Instr::Read(
                s.next_pos()
            ),
        Op::Write =>
            Instr::Write(
                s.next_arg(mode_1),
            ),
        Op::JumpIfTrue =>
            Instr::JumpIfTrue(
                s.next_arg(mode_1),
                s.next_arg(mode_2),
            ),
        Op::JumpIfFalse =>
            Instr::JumpIfFalse(
                s.next_arg(mode_1),
                s.next_arg(mode_2),
            ),
        Op::LessThan =>
            Instr::LessThan(
                s.next_arg(mode_1),
                s.next_arg(mode_2),
                s.next_pos()
            ),
        Op::Equal =>
            Instr::Equal(
                s.next_arg(mode_1),
                s.next_arg(mode_2),
                s.next_pos()
            ),
        Op::Halt =>
            Instr::Halt
    }
}

fn eval(s: &mut State) {
    let instr = parse_instruction(s);
    println!("{:?}", instr);
    match instr {
        Instr::Add(a, b, res) => {
            s.save(res, s.read_arg(a) + s.read_arg(b))
        },
        Instr::Mul(a, b, res) => {
            s.save(res, s.read_arg(a) * s.read_arg(b))
        },
        Instr::Read(res) => {
            let v = s.inputs.pop().expect("Out of inputs");
            println!("Read: {}", v);
            s.save(res, v)
        },
        Instr::JumpIfTrue(pred, res) => {
            if s.read_arg(pred) > 0 {
                println!("JUMP {:?}", res);
                s.pc = s.read_pos(res)
            }
        },
        Instr::JumpIfFalse(pred, res) => {
            if s.read_arg(pred) == 0 {
                println!("JUMP {:?}", res);
                s.pc = s.read_pos(res)
            }
        },
        Instr::LessThan(a, b, res) => {
            s.save(res, if s.read_arg(a) < s.read_arg(b) { 1 } else { 0 })
        },
        Instr::Equal(a, b, res) => {
            s.save(res, if s.read_arg(a) == s.read_arg(b) { 1 } else { 0 })
        },
        Instr::Write(a) => {
            let v = s.read_arg(a);
            println!("TEST WRITE {}", v);
            s.outputs.push(v);
        },
        Instr::Halt => {
            return
        }
    }
    eval(s);
}
