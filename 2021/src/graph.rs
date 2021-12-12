use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug)]
pub struct Graph<N> {
    map: BTreeMap<N, BTreeSet<N>>,
}

impl<N> Default for Graph<N> {
    fn default() -> Self {
        Graph {
            map: BTreeMap::default(),
        }
    }
}

impl<N: PartialEq + Ord + Copy> Graph<N> {
    pub fn insert(&mut self, a: N, b: N) {
        self.map.entry(a).or_insert_with(Default::default).insert(b);
        self.map.entry(b).or_insert_with(Default::default).insert(a);
    }

    pub fn neighbors(&self, n: N) -> impl Iterator<Item = N> + '_ {
        self.map.get(&n).unwrap().iter().copied()
    }
}

impl<N: Ord + Copy> FromIterator<(N, N)> for Graph<N> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (N, N)>,
    {
        let mut graph = Graph::default();
        for (a, b) in iter {
            graph.insert(a, b)
        }
        graph
    }
}
