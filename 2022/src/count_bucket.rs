#[derive(Debug, Clone)]
pub struct CountBucket<B: Bucketer> {
    map: Vec<usize>,
    phantom: std::marker::PhantomData<B>,
}

pub trait Bucketer {
    fn size() -> usize;
    fn bucket(&self) -> usize;
    fn unbucket(u: usize) -> Self;
}

impl Bucketer for char {
    fn size() -> usize {
        26
    }
    fn bucket(&self) -> usize {
        (*self as usize) - 65
    }
    fn unbucket(u: usize) -> Self {
        (u + 65) as u8 as char
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
    fn unbucket(_u: usize) -> Self {
        todo!()
    }
}

impl<B: Bucketer> Default for CountBucket<B> {
    fn default() -> CountBucket<B> {
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
    pub fn extend(&mut self, other: &CountBucket<B>) {
        for (i, v) in other.map.iter().enumerate() {
            self.map[i] += v;
        }
    }
}

#[test]
fn test() {
    let mut b: CountBucket<char> = CountBucket::default();
    b.count(&'A');
    b.count(&'A');
    b.count(&'Z');
}
