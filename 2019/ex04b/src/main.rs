fn main() {
    let min = 347312;
    let max = 805915;
    let mut matching_count = 0;
    for i in (min + 1)..max {
        let mut increasing: bool = true;
        let mut two_matching: bool = false;
        let mut prev_char: Option<char> = None;
        let mut count: Vec<(char, usize)> = Vec::new();
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
            if let Some(last) = count.last_mut() {
                if last.0 == c {
                    last.1 += 1
                } else {
                    count.push((c, 1))
                }
            } else {
                count.push((c, 1))
            }
        }
        if two_matching && increasing && count.iter().find(|p| p.1 == 2).is_some() {
            println!("{}", i);
            matching_count += 1;
        }
    }
    println!("matching: {}", matching_count);
}
