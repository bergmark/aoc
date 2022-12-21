use std::fs;

fn main() {
    let mut prog = fs::read_to_string("input.txt").expect("Could not read file").split(",").map(|s| s.parse::<i32>().unwrap()).collect::<Vec<i32>>();
    prog[1] = 12;
    prog[2] = 2;
    print!("{:?}", eval(prog, 0));
}

fn eval(mut prog: Vec<i32>, i: usize) -> Vec<i32> {
    match prog[i] {
        1 => {
            let a = prog[prog[i+1] as usize];
            let b = prog[prog[i+2] as usize];
            let c = prog[i+3] as usize;
            prog[c] = a + b;
            eval(prog, i + 4)
        },
        2 => {
            let a = prog[prog[i+1] as usize];
            let b = prog[prog[i+2] as usize];
            let c = prog[i+3] as usize;
            prog[c] = a * b;
            eval(prog, i + 4)
        },
        99 => prog,
        op => {
            panic!("Bad opcode {} at index {}", op, i)
        }
    }

}
