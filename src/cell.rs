use std::fmt::{Display, Formatter};

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Cell {
    pub number: usize,
    pub checked: bool,
}

impl Display for Cell {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let s = if self.checked { "X" } else { " " };
        write!(f, "{}{:2}", s, self.number)
    }
}

impl Cell {
    pub fn unchecked(number: usize) -> Cell {
        Cell {
            number,
            checked: false,
        }
    }
    pub fn check(&mut self) {
        assert!(!self.checked, "Cell was already checked");
        self.checked = true;
    }
}
