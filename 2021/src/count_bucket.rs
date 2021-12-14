pub use std::cmp::Ordering;

#[derive(Debug, Clone)]
pub struct CountBucket<B: Bucketer> {
    map: Vec<usize>,
    phantom: std::marker::PhantomData<B>,
}

pub trait Bucketer {
    fn size() -> usize;
    fn bucket(&self) -> usize;
}

impl Bucketer for char {
    fn size() -> usize {
        26
    }
    fn bucket(&self) -> usize {
        (*self as usize) - 65
    }
}
impl Bucketer for (char, char) {
    fn size() -> usize {
        26 * 26
    }
    fn bucket(&self) -> usize {
        let a = self.0 as usize - 65;
        let b = self.1 as usize - 65;
        a * 26 + b
    }
}

impl<B: Bucketer> CountBucket<B> {
    pub fn new() -> CountBucket<B> {
        CountBucket {
            map: vec![0; B::size()],
            phantom: std::marker::PhantomData,
        }
    }
}

impl<B: Bucketer> CountBucket<B> {
    pub fn get(&self, key: &impl Bucketer) -> usize {
        self.map[key.bucket()]
    }
    pub fn contains(&self, key: &impl Bucketer) -> bool {
        self.map[key.bucket()] != 0
    }
    pub fn count(&mut self, key: &impl Bucketer) {
        self.map[key.bucket()] += 1;
    }
    pub fn max(&self) -> usize {
        *self.map.iter().max().unwrap()
    }
    pub fn min(&self) -> usize {
        *self.map.iter().filter(|v| **v > 0).min().unwrap()
    }
}

#[test]
fn test() {
    let mut b: CountBucket<char> = CountBucket::new();
    b.count(&'A');
    b.count(&'A');
    b.count(&'Z');
}
