pub use anyhow::{anyhow, Context};
pub use itertools::Itertools;
pub use lazy_regex::regex;
pub use regex::Regex;
pub use regex_captures::Captures;
pub use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
pub use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
pub use std::convert::{TryFrom, TryInto};
pub use std::fmt;
use std::fmt::Debug;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{BufRead, BufReader, Lines};
pub use std::iter::FromIterator;
pub use std::marker::PhantomData;
pub use std::ops::{Add, AddAssign, Mul, RangeInclusive, Sub, SubAssign};
use std::path::Path;
pub use std::str::FromStr;

pub mod board;
pub mod cell;
pub mod count;
pub mod count_bucket;
pub mod direction;
pub mod graph;
pub mod grid;
pub mod job_queue;
pub mod job_queue2;
pub mod job_queue_set;
pub mod line;
pub mod point;

pub use self::{
    count::Count,
    count_bucket::{Bucketer, CountBucket},
    direction::Direction,
    graph::Graph,
    grid::{Grid, GridDisplay},
    job_queue::JobQueue,
    job_queue2::JobQueue2,
    job_queue_set::JobQueueSet,
    line::Line,
    point::Point,
};

pub fn read_to_string<P>(filename: P) -> String
where
    P: AsRef<Path>,
{
    std::fs::read_to_string(filename).unwrap()
}

pub fn read_lines<P>(filename: P) -> Lines<BufReader<File>>
where
    P: AsRef<Path> + std::fmt::Display,
{
    let file = File::open(&filename).unwrap_or_else(|e| panic!("Error opening {filename}: {e}"));
    BufReader::new(file).lines()
}

pub fn read_parsed<'a, A: 'a + FromStr>(filename: &'a str) -> impl Iterator<Item = A> + 'a
where
    A::Err: Debug,
{
    read_parsed_res(filename).map(|a| a.unwrap())
}

pub fn read_parsed_with<'a, A: 'a, F>(filename: &'a str, parse: F) -> impl Iterator<Item = A> + 'a
where
    F: 'a + Fn(&str) -> A,
{
    read_lines(filename).map(move |a| parse(&a.unwrap()))
}

pub fn read_parsed_res<A: FromStr, P: AsRef<Path> + std::fmt::Display>(
    filename: P,
) -> impl Iterator<Item = Result<A, String>>
where
    A::Err: Debug,
{
    read_lines(filename).map(|p| {
        let p = p.unwrap();
        p.parse().map_err(move |_| p)
    })
}

pub fn eq<A: PartialEq>(x: A, y: A) -> bool {
    x == y
}

#[allow(clippy::result_unit_err)]
pub fn split2<'a>(s: &'a str, sep: &str) -> Result<(&'a str, &'a str), ()> {
    let v: Vec<_> = s.split(sep).collect();
    Ok((v.first().ok_or(())?, v.get(1).ok_or(())?))
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

#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct Hsh(u64);

pub fn hash<T: Hash>(t: &T) -> Hsh {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    Hsh(s.finish())
}

pub struct SpaceSep<A>(pub Vec<A>);

impl<A: FromStr> FromStr for SpaceSep<A> {
    type Err = <A as FromStr>::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(SpaceSep(
            s.split(' ')
                .map(|s| s.parse())
                .collect::<Result<Vec<_>, _>>()?,
        ))
    }
}

pub struct Chars(pub Vec<char>);
impl FromStr for Chars {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.chars().collect()))
    }
}
pub struct Digs(pub Vec<u32>);
impl FromStr for Digs {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(x) = s.chars().map(|c| c.to_digit(10)).collect() {
            Ok(Self(x))
        } else {
            Err(())
        }
    }
}
