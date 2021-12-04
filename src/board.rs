use crate::cell::Cell;

#[derive(Debug, Eq, PartialEq)]
pub struct Board {
    pub board: Vec<Vec<Cell>>,
}

impl Board {
    pub fn check(&mut self, number: usize) -> bool {
        check_vec(&mut self.board[0], number)
            || check_vec(&mut self.board[1], number)
            || check_vec(&mut self.board[2], number)
            || check_vec(&mut self.board[3], number)
            || check_vec(&mut self.board[4], number)
    }

    pub fn is_complete(&self) -> bool {
        0 == count_unchecked(&self.board[0])
            || 0 == count_unchecked(&self.board[1])
            || 0 == count_unchecked(&self.board[2])
            || 0 == count_unchecked(&self.board[3])
            || 0 == count_unchecked(&self.board[4])
            || 0 == count_unchecked_col(&self.board, 0)
            || 0 == count_unchecked_col(&self.board, 1)
            || 0 == count_unchecked_col(&self.board, 2)
            || 0 == count_unchecked_col(&self.board, 3)
            || 0 == count_unchecked_col(&self.board, 4)
    }
}

fn check_vec(col: &mut Vec<Cell>, number: usize) -> bool {
    if let Some(cell) = col.iter_mut().find(|cell| cell.number == number) {
        cell.check();
        true
    } else {
        false
    }
}

fn count_unchecked(col: &Vec<Cell>) -> usize {
    col.iter()
        .map(|cell| if cell.checked { 0 } else { 1 })
        .sum()
}

fn count_unchecked_col(board: &Vec<Vec<Cell>>, col: usize) -> usize {
    board
        .iter()
        .map(|row| if row[col].checked { 0 } else { 1 })
        .sum()
}
