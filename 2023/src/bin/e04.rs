use aoc2023::*;
use regex_captures::Captures;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s04.txt"), 13);
    assert_eq!(a("txt/e04.txt"), 15205);
    assert_eq!(b("txt/s04.txt"), 30);
    assert_eq!(b("txt/e04.txt"), 6189740);
}

fn a(s: &str) -> usize {
    read_parsed::<Card>(s)
        .map(|card| {
            card.have
                .intersection(&card.winning)
                .enumerate()
                .last()
                .map(|(i, _)| 2_usize.pow(i as u32))
                .unwrap_or(0)
        })
        .sum()
}

fn b(s: &str) -> usize {
    let mut wins: HashMap<CardId, Set> = Default::default();
    let mut card_count: Count<CardId> = Count::default();
    let mut total_counts: Count<CardId> = Count::default();

    for card in read_parsed::<Card>(s) {
        card_count.count(card.id);

        wins.entry(card.id).or_default();

        for win in card
            .have
            .intersection(&card.winning)
            .enumerate()
            .map(|(i, _)| CardId(card.id.0 + i + 1))
        {
            wins.entry(card.id).and_modify(|v| {
                v.insert(win);
            });
        }
    }

    while !card_count.is_empty() {
        let mut new_card_count: Count<CardId> = Count::default();
        for (&id, count) in std::mem::take(&mut card_count).iter() {
            total_counts.increment(id, count);
            for &win in wins.get(&id).unwrap() {
                new_card_count.increment(win, count)
            }
        }
        card_count = new_card_count;
    }
    total_counts.counts().sum()
}

type Set = HashSet<CardId>;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
struct CardId(usize);

#[derive(Debug)]
struct Card {
    id: CardId,
    have: Set,
    winning: Set,
}

impl FromStr for Card {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        let cap = Captures::new(
            regex!(r#"^Card +(?P<id>\d+): (?P<have>[^|]+) \| (?P<winning>[^|]+)$"#),
            s,
        )
        .unwrap();
        let id = CardId(cap.parse_name("id").unwrap());

        let ns = |s: String| {
            regex!(r#"\s+"#)
                .split(s.trim())
                .map(|n| n.parse().map(CardId).unwrap())
                .collect()
        };

        let have = ns(cap.parse_name("have").unwrap());
        let winning = ns(cap.parse_name("winning").unwrap());
        Ok(Card { id, have, winning })
    }
}
