use std::fs;

fn main() {
    let prog = fs::read_to_string("input.txt").expect("Could not read file").split(",").map(|s| s.parse::<i32>().unwrap()).collect::<Vec<i32>>();
    let (noun, verb) = find_result(&prog, 19690720);
    let answer = 100 * noun + verb;
    print!("{:?}", answer);
}

fn find_result(orig: &Vec<i32>, expected: i32) -> (i32, i32) {
    for noun in 0..99 {
        for verb in 0..99 {
            let mut prog = orig.clone();
            prog[1] = noun;
            prog[2] = verb;
            let res = eval(prog, 0);
            if res[0] == expected {
                return (noun, verb)
            }
        }
    };
    return (-1, -1)
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
