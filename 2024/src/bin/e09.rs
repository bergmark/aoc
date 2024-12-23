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

fn checksum(disk: &[E]) -> usize {
    disk.iter().enumerate().map(|(i, &v)| i * (v.file_id().unwrap_or(0) as usize)).sum()
}
