use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s12.txt"), 31);
    assert_eq!(a("txt/e12.txt"), 339);
    assert_eq!(b("txt/s12.txt"), 29);
    assert_eq!(b("txt/e12.txt"), 332);
}

fn a(s: &str) -> usize {
    let state = parse(s);
    let path = find_tile(&state.grid, state.start, state.end).unwrap();
    path.len()
}

fn b(s: &str) -> usize {
    let state = parse(s);
    state
        .grid
        .points()
        .filter(|&(_, v)| v == 'a')
        .filter_map(|(start, _)| find_tile(&state.grid, start, state.end))
        .map(|path| path.len())
        .min()
        .unwrap()
}

struct State {
    start: Point,
    end: Point,
    grid: Grid<char>,
}

fn parse(s: &str) -> State {
    let mut grid: Grid<char> = Grid::new(
        read_lines(s)
            .map(|l| l.unwrap().chars().collect::<Vec<_>>())
            .collect(),
    );
    let start = grid.points().find(|&(_, a)| a == 'S').unwrap().0;
    let end = grid.points().find(|&(_, a)| a == 'E').unwrap().0;
    *grid.get_mut(start).unwrap() = 'a';
    *grid.get_mut(end).unwrap() = 'z';
    State { start, end, grid }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Node {
    distance: usize,
    parent: Option<Point>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct T {
    parent: Option<Point>,
    point: Point,
}

pub fn find_tile(grid: &Grid<char>, start: Point, end: Point) -> Option<VecDeque<Point>> {
    let is_target = |p: Point| p == end;
    let can_walk = |curr: char, dest: char| curr as u8 >= (dest as u8) - 1;

    let mut distances: Grid<Option<Node>> = Grid::init(grid.len(), None);
    let mut last_distance = None;
    let mut queue: HashMap<Point, Option<Point>> = Default::default();
    let mut end: Option<T> = None;
    queue.insert(start, None);

    'w: while !queue.is_empty() {
        let distance = last_distance.map_or(0, |v| v + 1);

        for (point, parent) in std::mem::take(&mut queue) {
            if is_target(point) {
                end = Some(T { parent, point });
                break 'w;
            }
            {
                let v = distances.get_mut(point).unwrap();
                if v.is_none() {
                    *v = Some(Node { parent, distance });
                }
            }
            let tile_val = grid.get(point).unwrap();

            for (neighbor_point, neighbor_val) in grid.straight_neighbors(point) {
                if can_walk(tile_val, neighbor_val)
                    && distances.get(neighbor_point).unwrap().is_none()
                {
                    queue.insert(neighbor_point, Some(point));
                }
            }
        }

        last_distance = Some(distance);
    }

    end.map(|T { mut parent, point }| {
        let mut point = Some(point);
        let mut path = VecDeque::new();
        while let Some(this_point) = point {
            if this_point != start {
                path.push_front(this_point);
            }
            if let Some(this_parent) = parent {
                let next = distances.get(this_parent).unwrap().unwrap();
                point = Some(this_parent);
                parent = next.parent;
            } else {
                point = None;
                parent = None;
            }
        }
        path
    })
}
