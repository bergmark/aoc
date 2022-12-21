#![feature(vec_remove_item)]
use std::fs;
use std::cmp;
use std::env;
use permute::permutations_of;

fn read_file(fp: &String) -> Vec<i32> {
    parse(&fs::read_to_string(fp).expect("Could not read file"))
}

fn parse(s: &String) -> Vec<i32> {
    s.trim().split(',').map(|s| s.parse().unwrap()).collect()
}

fn main() {

    let phase_settings = [0,1,2,3,4];

    let args = env::args().collect::<Vec<String>>();
    let input_file = &args[1];
    println!("file: {}", input_file);

    let prog = read_file(&input_file);

    let mut max_output_signal = 0;
    for permutation in permutations_of(&phase_settings) {
        let mut amplifiers: Vec<_> = permutation.zip(0..).map(|(phase_setting,amp)| {
            State::new(format!("{},{}", amp, *phase_setting), prog.clone(), vec![*phase_setting])
        }).collect::<Vec<_>>();
        amplifiers.push(State::new("Dummy".to_string(), prog.clone(), vec![])); // dummy process to consume outputs of last amplifier
        amplifiers[0].add_input(0);

        let mut alive = vec![0,1,2,3,4];

        while !alive.is_empty() {
            for i in alive.clone() {
                let amp = &mut amplifiers[i];
                match amp.status {
                    Status::Ok => {
                        step(amp);
                        if let Some(o) = amp.outputs.pop() {
                            amplifiers[i+1].add_input(o)
                        }
                    },
                    Status::Halt => {
                        alive.remove_item(&i);
                        println!("{} STOPPED", i);
                    },
                    Status::NeedInput => {
                        if !amp.inputs.is_empty() {
                            step(amp);
                            if let Some(o) = amp.outputs.pop() {
                                amplifiers[i+1].add_input(o)
                            }
                        } else {
                            println!("{} waiting", amp.label);
                        }
                    }
                }
            }
        }

        assert!(amplifiers[5].inputs.len() == 1);
        let last_output = *amplifiers[5].inputs.last().expect("last output");
        println!("");
        println!("output {}", last_output);
        println!("");
        max_output_signal = cmp::max(max_output_signal, last_output);

    }

    println!("Max Thruster Signal: {}", max_output_signal);

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
    label: String,
    pc: Pos,
    status: Status,
    memory: Vec<i32>,
    inputs: Vec<i32>,
    outputs: Vec<i32>
}
impl State {

    fn new(label: String, m: Vec<i32>, inputs: Vec<i32>) -> State {
        State {
            label: label,
            pc: Pos(0),
            status: Status::Ok,
            memory: m,
            inputs: inputs,
            outputs: vec![]
        }
    }

    fn next(&mut self) -> i32 {
        self.pc = Pos(self.pc.0 + 1);
        self.memory[self.pc.0 - 1]
    }

    fn add_input(&mut self, i: i32) {
        self.inputs.push(i)
    }

    fn next_arg(&mut self, mode: Mode) -> Arg {
        Arg::new(mode, self.next())
    }

    fn next_pos(&mut self) -> Pos {
        Pos(self.next() as usize)
    }

    fn save(&mut self, p: Pos, v: i32) {
        println!("{} memory[{:?}] = {}", self.label, p.0, v);
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

pub enum Status {
    Ok,
    Halt,
    NeedInput
}

fn step(s: &mut State) {
    let instr = parse_instruction(s);
    println!("{} EVAL {:?}", s.label, instr);
    let status = match instr {
        Instr::Add(a, b, res) => {
            s.save(res, s.read_arg(a) + s.read_arg(b));
            Status::Ok
        },
        Instr::Mul(a, b, res) => {
            s.save(res, s.read_arg(a) * s.read_arg(b));
            Status::Ok
        },
        Instr::Read(res) => {
            if s.inputs.is_empty() {
                println!("{} NEED INPUT", s.label);
                s.pc = Pos(s.pc.0 - 2);
                Status::NeedInput
            } else {
                let v = s.inputs.pop().expect("Out of inputs");
                println!("{} READ: {}", s.label, v);
                s.save(res, v);
                Status::Ok
            }
        },
        Instr::JumpIfTrue(pred, res) => {
            if s.read_arg(pred) > 0 {
                println!("{} JUMP: {:?}", s.label, res);
                s.pc = s.read_pos(res);
            }
            Status::Ok
        },
        Instr::JumpIfFalse(pred, res) => {
            if s.read_arg(pred) == 0 {
                println!("{} JUMP: {:?}", s.label, res);
                s.pc = s.read_pos(res);
            }
            Status::Ok
        },
        Instr::LessThan(a, b, res) => {
            s.save(res, if s.read_arg(a) < s.read_arg(b) { 1 } else { 0 });
            Status::Ok
        },
        Instr::Equal(a, b, res) => {
            s.save(res, if s.read_arg(a) == s.read_arg(b) { 1 } else { 0 });
            Status::Ok
        },
        Instr::Write(a) => {
            let v = s.read_arg(a);
            println!("{} WRITE: {}", s.label, v);
            s.outputs.push(v);
            Status::Ok
        },
        Instr::Halt => {
            Status::Halt
        }
    };

    s.status = status
}
