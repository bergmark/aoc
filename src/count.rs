use std::collections::HashMap;
use std::hash::Hash;
pub use std::cmp::Ordering;

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
