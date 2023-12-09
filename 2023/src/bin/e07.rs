use aoc2023::*;
use once_cell::sync::Lazy;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s07.txt"), 6440);
    assert_eq!(a("txt/e07.txt"), 248113761);
    assert_eq!(b("txt/s07.txt"), 5905);
    assert_eq!(b("txt/e07.txt"), 246285222);
}

#[derive(Clone, Debug)]
struct Hand {
    ranks: Vec<usize>,
    bet: usize,
    typ: Type,
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
struct Card(char);

impl Card {
    fn rank(self, is_a: bool) -> usize {
        let a: Lazy<BTreeMap<char, usize>> = Lazy::new(|| {
            "AKQJT98765432"
                .chars()
                .rev()
                .enumerate()
                .map(|(i, c)| (c, i))
                .collect()
        });
        let b: Lazy<BTreeMap<char, usize>> = Lazy::new(|| {
            "AKQT98765432J"
                .chars()
                .rev()
                .enumerate()
                .map(|(i, c)| (c, i))
                .collect()
        });
        if is_a {
            *a.get(&self.0).unwrap()
        } else {
            *b.get(&self.0).unwrap()
        }
    }
}

fn hand_from_str(is_a: bool, s: &str) -> Hand {
    let hand_bet: Vec<&str> = s.split(' ').collect();
    let mut cards: Count<Card> = Count::default();
    let mut jokers: usize = 0;
    let mut ranks: Vec<usize> = vec![];
    for card in hand_bet[0].chars() {
        let card = Card(card);
        ranks.push(card.rank(is_a));
        if card == Card('J') {
            jokers += 1;
        }
        if is_a || card != Card('J') {
            cards.count(card);
        }
    }
    let bet = hand_bet[1].parse().unwrap();
    let typ = if is_a {
        hand_type_a(&cards)
    } else {
        hand_type_b(&cards, jokers)
    };
    Hand { bet, typ, ranks }
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
enum Type {
    FiveKind,
    FourKind,
    FullHouse,
    ThreeKind,
    TwoPair,
    OnePair,
    High,
}

fn hand_type_a(c: &Count<Card>) -> Type {
    use Type::*;
    let max = c.max().unwrap().1;
    if c.len() == 1 {
        Type::FiveKind
    } else if c.len() == 2 {
        if max == 4 {
            FourKind
        } else if max == 3 {
            FullHouse
        } else {
            unreachable!()
        }
    } else if max == 3 {
        ThreeKind
    } else if max == 2 {
        if c.maxes().unwrap().0.len() == 2 {
            TwoPair
        } else {
            OnePair
        }
    } else {
        High
    }
}

fn hand_type_b(c: &Count<Card>, jokers: usize) -> Type {
    use Type::*;
    let distinct = c.counts().count();
    let t = if jokers == 5 || jokers == 4 {
        FiveKind
    } else if jokers == 3 {
        if distinct == 2 {
            FourKind
        } else if distinct == 1 {
            FiveKind
        } else {
            unreachable!()
        }
    } else if jokers == 2 {
        if distinct == 3 {
            ThreeKind
        } else if distinct == 2 {
            FourKind
        } else if distinct == 1 {
            FiveKind
        } else {
            unreachable!()
        }
    } else if jokers == 1 {
        if distinct == 1 {
            FiveKind
        } else if distinct == 2 {
            let max_count = c.max().unwrap().1;
            if max_count == 3 {
                FourKind
            } else if max_count == 2 {
                FullHouse
            } else {
                unreachable!()
            }
        } else if distinct == 3 {
            ThreeKind
        } else if distinct == 4 {
            OnePair
        } else {
            unreachable!()
        }
    } else if jokers == 0 {
        hand_type_a(c)
    } else {
        unreachable!()
    };
    t
}

fn cmp_hand(a: &Hand, b: &Hand) -> Ordering {
    match a.typ.cmp(&b.typ) {
        Ordering::Equal => b.ranks.cmp(&a.ranks),
        e => e,
    }
}

fn a(s: &str) -> usize {
    let mut s: Vec<Hand> = read_parsed::<String>(s)
        .map(|s| hand_from_str(true, &s))
        .collect();
    s.sort_by(|a, b| cmp_hand(b, a));
    s.into_iter()
        .enumerate()
        .map(|(mut i, h)| {
            i += 1;
            h.bet * i
        })
        .sum()
}

fn b(s: &str) -> usize {
    let mut s: Vec<Hand> = read_parsed::<String>(s)
        .map(|s| hand_from_str(false, &s))
        .collect();
    s.sort_by(|a, b| cmp_hand(b, a));
    s.into_iter()
        .enumerate()
        .map(|(mut i, h)| {
            i += 1;
            h.bet * i
        })
        .sum::<usize>()
}
