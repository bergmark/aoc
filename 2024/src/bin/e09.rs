use aoc2024::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s09.txt"), 1928);
    assert_eq!(a("txt/e09.txt"), 6283404590840);
    assert_eq!(b("txt/s09.txt"), 2858);
    assert_eq!(b("txt/e09.txt"), 6304576012713);
}

#[derive(Copy, Clone, Debug)]
enum E {
    Free,
    File(u32),
}

impl E {
    fn is_free(self) -> bool {
        matches!(self, E::Free)
    }
    fn is_occupied(self) -> bool {
        matches!(self, E::File(_))
    }
    fn file_id(self) -> Option<u32> {
        match self {
            E::Free => None,
            E::File(id) => Some(id),
        }
    }
}

fn a(s: &str) -> usize {
    let line: Vec<u32> = read_parsed::<Digs>(s).next().unwrap().0;
    let mut disk: Vec<E> = vec![];
    let mut id = 0;
    let mut is_file = true;
    for dig in line {
        let el = if is_file { E::File(id) } else { E::Free };
        for _ in 0..dig {
            disk.push(el);
        }
        if is_file {
            id += 1;
        }
        is_file = !is_file;
    }
    let mut i = 0;
    'loo: for j in (0..disk.len()).rev() {
        if disk[j].is_free() {
            continue 'loo;
        }
        while disk[i].is_occupied() {
            i += 1;
        }
        if i >= j {
            break 'loo;
        }
        disk.swap(i, j);
    }
    checksum(&disk)
}

#[derive(Copy, Clone, Debug)]
struct Range {
    start: usize,
    len: usize,
    e: E,
}

fn b(s: &str) -> usize {
    let line: Vec<u32> = read_parsed::<Digs>(s).next().unwrap().0;
    let mut id = 0;
    let mut is_file = true;
    let mut ranges: Vec<Range> = vec![];
    let mut start = 0;
    for dig in line {
        let len = dig as usize;
        let e = if is_file {
            id += 1;
            E::File(id - 1)
        } else {
            E::Free
        };
        if dig > 0 {
            ranges.push(Range { start, len, e });
            start += len;
        }
        is_file = !is_file;
    }
    let mut j = ranges.len() - 1;
    let mut tried: BTreeSet<u32> = Default::default();
    'loo: loop {
        let rj = ranges[j];
        if j == 0 {
            break 'loo;
        }
        let id = match rj.e {
            E::File(id) => id,
            E::Free => {
                j -= 1;
                continue 'loo;
            }
        };
        if !tried.insert(id) {
            j -= 1;
            continue 'loo;
        }
        let mut i = 0;
        'w: while i < j {
            if ranges[i].e.is_occupied() {
                i += 1;
                continue 'w;
            } else if ranges[j].len <= ranges[i].len {
                ranges_move(&mut ranges, i, j);
                j = ranges.len() - 1;
                continue 'loo;
            } else {
                i += 1;
                continue 'w;
            }
        }
        if j > 0 {
            j -= 1;
        } else {
            break 'loo;
        }
    }

    checksum_ranges(&ranges)
}

fn checksum_ranges(ranges: &Vec<Range>) -> usize {
    let mut sum = 0;
    let mut i = 0;
    for Range { start, len, e } in ranges {
        if let E::File(id) = e {
            for _ in *start..(start + len) {
                let id = *id as usize;
                sum += i * id;
                i += 1;
            }
        } else {
            i += len;
        }
    }
    sum
}

fn ranges_move(ranges: &mut Vec<Range>, i: usize, j: usize) {
    let free = ranges[i];
    let file = ranges[j];
    ranges[i] = ranges[j];
    ranges[j].start = i;
    ranges[j] = Range {
        start: ranges[j].start,
        len: ranges[j].len,
        e: E::Free,
    };
    if free.len > file.len {
        ranges.insert(
            i + 1,
            Range {
                start: free.start + file.len,
                len: free.len - file.len,
                e: E::Free,
            },
        );
        merge_free_ranges(ranges)
    }
}

fn merge_free_ranges(ranges: &mut Vec<Range>) {
    let mut i = 0;
    'w: while i < ranges.len() - 1 {
        if i + 1 == ranges.len() {
            break 'w;
        }
        if ranges[i].e.is_free() && ranges[i + 1].e.is_free() {
            let rest = ranges.remove(i + 1);
            ranges[i].len += rest.len;
        } else {
            i += 1;
        }
    }
}

fn checksum(disk: &[E]) -> usize {
    disk.iter()
        .enumerate()
        .map(|(i, &v)| i * (v.file_id().unwrap_or(0) as usize))
        .sum()
}
