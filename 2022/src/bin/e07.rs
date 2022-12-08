use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s07.txt"), 95437);
    assert_eq!(a("txt/e07.txt"), 1583951);
    assert_eq!(b("txt/s07.txt"), 24933642);
    assert_eq!(b("txt/e07.txt"), 214171);
}

fn a(s: &str) -> usize {
    let dirs = parse(s);

    let mut sizes: HashMap<VecDeque<String>, usize> = HashMap::new();

    let mut dirs_to_check: BTreeSet<VecDeque<String>> = dirs.keys().cloned().collect();

    while !dirs_to_check.is_empty() {
        'foo: for dir_to_check in std::mem::take(&mut dirs_to_check) {
            let mut dir_size = 0;
            for content in dirs.get(&dir_to_check).unwrap() {
                match content {
                    LsResult::Dir { name } => {
                        let mut subdir = dir_to_check.clone();
                        subdir.push_back(name.clone());
                        if let Some(subdir_size) = sizes.get(&subdir) {
                            dir_size += subdir_size;
                        } else {
                            dirs_to_check.insert(dir_to_check);
                            continue 'foo;
                        }
                    }
                    LsResult::File { size } => {
                        dir_size += size;
                    }
                }
            }
            sizes.insert(dir_to_check.clone(), dir_size);
        }
    }

    sizes.values().filter(|size| **size <= 100000).sum()
}

fn b(s: &str) -> usize {
    let dirs = parse(s);

    let mut sizes: HashMap<VecDeque<String>, usize> = HashMap::new();

    let mut dirs_to_check: BTreeSet<VecDeque<String>> = dirs.keys().cloned().collect();

    while !dirs_to_check.is_empty() {
        'foo: for dir_to_check in std::mem::take(&mut dirs_to_check) {
            let mut dir_size = 0;
            for content in dirs.get(&dir_to_check).unwrap() {
                match content {
                    LsResult::Dir { name } => {
                        let mut subdir = dir_to_check.clone();
                        subdir.push_back(name.clone());
                        if let Some(subdir_size) = sizes.get(&subdir) {
                            dir_size += subdir_size;
                        } else {
                            dirs_to_check.insert(dir_to_check);
                            continue 'foo;
                        }
                    }
                    LsResult::File { size } => {
                        dir_size += size;
                    }
                }
            }
            sizes.insert(dir_to_check.clone(), dir_size);
        }
    }
    let avail = 70000000;
    let need_unused = 30000000;
    let used = sizes.get(&VecDeque::new()).unwrap();
    let unused = avail - used;
    let need = need_unused - unused;
    *sizes.values().filter(|size| **size >= need).min().unwrap()
}

type Fs = HashMap<VecDeque<String>, Vec<LsResult>>;

fn parse(s: &str) -> Fs {
    let mut dirs: HashMap<VecDeque<String>, Vec<LsResult>> = HashMap::new();
    let mut wd: VecDeque<String> = Default::default();
    for row in read_parsed::<Row>(s) {
        match row {
            Row::Noop => {}
            Row::Command(command) => match command {
                Command::Cd { dir } => {
                    if dir == ".." {
                        wd.pop_back().unwrap();
                    } else if dir == "/" {
                        wd = Default::default();
                    } else {
                        wd.push_back(dir);
                    }
                }
                Command::Ls => {}
            },
            Row::LsResult(ls_result) => {
                let e = dirs.entry(wd.clone()).or_default();
                e.push(ls_result);
            }
        }
    }
    dirs
}

#[derive(Debug, Clone)]
enum LsResult {
    Dir { name: String },
    File { size: usize },
}

#[derive(Debug, Clone)]
enum Row {
    Command(Command),
    LsResult(LsResult),
    Noop,
}

#[derive(Debug, Clone)]
enum Command {
    Cd { dir: String },
    Ls,
}

impl FromStr for Row {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        if let Ok(cap) = Captures::new(regex!(r#"^\$ cd (.+)$"#), s) {
            let dir = cap.get(1).unwrap();
            Ok(Row::Command(Command::Cd { dir }))
        } else if let Ok(_cap) = Captures::new(regex!(r#"^\$ ls$"#), s) {
            Ok(Row::Command(Command::Ls))
        } else if let Ok(cap) = Captures::new(regex!(r#"^dir (.+)$"#), s) {
            Ok(Row::LsResult(LsResult::Dir {
                name: cap.get(1).unwrap(),
            }))
        } else if let Ok(cap) = Captures::new(regex!(r#"^(\d+) .+$"#), s) {
            Ok(Row::LsResult(LsResult::File {
                size: cap.parse(1).unwrap(),
            }))
        } else if s.is_empty() {
            Ok(Row::Noop)
        } else {
            panic!("{s}")
        }
    }
}
