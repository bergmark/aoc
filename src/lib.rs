pub use itertools::Itertools;
pub use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
pub use std::collections::{HashMap, HashSet};
pub use std::convert::TryFrom;
pub use std::convert::TryInto;
pub use std::fmt;
use std::fmt::Debug;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{BufRead, BufReader, Lines};
pub use std::iter::FromIterator;
pub use std::marker::PhantomData;
pub use std::ops::{Add, AddAssign, Mul, Sub, SubAssign};
use std::path::Path;
pub use std::str::FromStr;

pub mod direction;
pub mod point;

pub fn read_lines<P>(filename: P) -> Lines<BufReader<File>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename).unwrap();
    BufReader::new(file).lines()
}

pub fn read_parsed<'a, A: 'a + FromStr>(filename: &'a str) -> impl Iterator<Item = A> + 'a
where
    A::Err: Debug,
{
    read_parsed_opt(filename).map(|a| a.unwrap())
}

pub fn read_parsed_opt<A: FromStr, P: AsRef<Path>>(filename: P) -> impl Iterator<Item = Option<A>>
where
    A::Err: Debug,
{
    read_lines(filename).map(|p| p.unwrap().parse().ok())
}

pub fn eq<A: PartialEq>(x: A, y: A) -> bool {
    x == y
}

#[allow(clippy::result_unit_err)]
pub fn split2<'a>(s: &'a str, sep: &str) -> Result<(&'a str, &'a str), ()> {
    let v: Vec<_> = s.split(sep).collect();
    Ok((v.get(0).ok_or(())?, v.get(1).ok_or(())?))
}

pub fn take_while(s: &str, p: impl Fn(char) -> bool) -> (String, String) {
    let mut go = true;
    let mut a = String::new();
    let mut b = String::new();
    for c in s.chars() {
        if go {
            if p(c) {
                a.push(c);
            } else {
                go = false;
                b.push(c);
            }
        } else {
            b.push(c)
        }
    }
    (a, b)
}

#[test]
fn test_take_while() {
    assert_eq!(
        take_while("#123abc", |c| c == '#'),
        ("#".to_owned(), "123abc".to_owned())
    );
    assert_eq!(
        take_while("123abc", |c| c == '#'),
        ("".to_owned(), "123abc".to_owned())
    );
}

#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub struct Hsh(u64);

pub fn hash<T: Hash>(t: &T) -> Hsh {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    Hsh(s.finish())
}

#[derive(Default, Debug)]
pub struct Count<K>(HashMap<K, usize>);

impl<K: Eq + Hash> Count<K> {
    pub fn get(&self, key: &K) -> usize {
        *self.0.get(key).unwrap_or(&0)
    }
    pub fn count(&mut self, key: K) {
        self.0.entry(key).and_modify(|i| *i += 1).or_insert(1);
    }
    pub fn counts(&self) -> impl Iterator<Item = usize> + '_ {
        self.0.values().copied()
    }
    pub fn max(&self) -> Option<(&K, usize)> {
        self.maxes().map(|(ks, v)| (ks[0], v))
    }
    pub fn maxes(&self) -> Option<(Vec<&K>, usize)> {
        self.pick(Ordering::Greater)
    }
    pub fn min(&self) -> Option<(&K, usize)> {
        self.mins().map(|(ks, v)| (ks[0], v))
    }
    pub fn mins(&self) -> Option<(Vec<&K>, usize)> {
        self.pick(Ordering::Less)
    }

    pub fn pick(&self, ord: Ordering) -> Option<(Vec<&K>, usize)> {
        let mut pick_keys = vec![];
        let mut pick_v: Option<usize> = None;
        for (k, v) in self.0.iter() {
            match pick_v {
                None => {
                    pick_v = Some(*v);
                    pick_keys.push(k);
                }
                Some(prev) => {
                    let o = v.cmp(&prev);
                    if o == ord {
                        pick_v = Some(*v);
                        pick_keys = vec![k];
                    } else if *v == prev {
                        pick_keys.push(k);
                    }
                }
            }
        }
        pick_v.map(|v| (pick_keys, v))
    }
}
