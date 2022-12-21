fn main() {
    let min = 347312;
    let max = 805915;
    let mut matching_count = 0;
    for i in (min + 1)..max {
        let mut increasing: bool = true;
        let mut two_matching: bool = false;
        let mut prev_char: Option<char> = None;
        for c in i.to_string().chars() {
            let pc = prev_char;
            prev_char = Some(c);
            if let Some(pcc) = pc {
                if pcc == c {
                    two_matching = true;
                }
                if pcc > c {
                    increasing = false;
                }
            }
        }
        if two_matching && increasing {
            println!("{}", i);
            matching_count += 1;
        }
    }
    println!("matching: {}", matching_count);
}
