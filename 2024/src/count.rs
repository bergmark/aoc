pub use std::cmp::Ordering;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone)]
pub struct Count<K> {
    map: HashMap<K, usize>,
    max: Option<(K, usize)>,
}

impl<K> Default for Count<K> {
    fn default() -> Self {
        Count {
            map: Default::default(),
            max: None,
        }
    }
}
impl<K: Eq + Hash + Clone> Count<K> {
    pub fn get(&self, key: &K) -> usize {
        *self.map.get(key).unwrap_or(&0)
    }
    pub fn contains(&self, key: &K) -> bool {
        self.map.get(key).is_some()
    }
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
    pub fn count(&mut self, key: K) {
        self.increment(key, 1);
    }
    pub fn increment(&mut self, key: K, inc: usize) {
        let new = *self
            .map
            .entry(key.clone())
            .and_modify(|i| *i += inc)
            .or_insert(inc);
        match self.max {
            None => self.max = Some((key, new)),
            Some((_, max_v)) => {
                if new > max_v {
                    self.max = Some((key, new))
                }
            }
        }
    }
    pub fn counts(&self) -> impl Iterator<Item = usize> + '_ {
        self.map.values().copied()
    }
    pub fn keys(&self) -> impl Iterator<Item = &K> + '_ {
        self.map.keys()
    }
    pub fn iter(&self) -> impl Iterator<Item = (&K, usize)> + '_ {
        self.map.iter().map(|(k, &v)| (k, v))
    }
    pub fn iter_from_max(&self) -> Vec<(&K, &usize)> {
        let mut v: Vec<_> = self.map.iter().collect();
        v.sort_by(|(_, v), (_, v2)| v2.cmp(v));
        v
    }
    pub fn max(&self) -> Option<(&K, usize)> {
        self.max.as_ref().map(|(k, v)| (k, *v))
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
    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn pick(&self, ord: Ordering) -> Option<(Vec<&K>, usize)> {
        let mut pick_keys = vec![];
        let mut pick_v: Option<usize> = None;
        for (k, v) in self.map.iter() {
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
