use crate::cell::Cell;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Board {
    pub board: Vec<Vec<Cell>>,
}

impl Board {
    pub fn check(&mut self, number: usize) -> bool {
        for i in 0..self.board.len() {
            for j in 0..self.board[0].len() {
                if self.board[i][j].number == number {
                    self.board[i][j].check();
                    return true;
                }
            }
        }
        false
    }

    pub fn is_complete(&self) -> bool {
        for row in &self.board {
            if 0 == count_unchecked_row(row) {
                return true;
            }
        }
        for col_i in 0..self.board[0].len() {
            if 0 == count_unchecked_col(&self.board, col_i) {
                return true;
            }
        }
        false
    }
}

fn count_unchecked_row(col: &[Cell]) -> usize {
    col.iter()
        .map(|cell| if cell.checked { 0 } else { 1 })
        .sum()
}

fn count_unchecked_col(board: &[Vec<Cell>], col: usize) -> usize {
    board
        .iter()
        .map(|row| if row[col].checked { 0 } else { 1 })
        .sum()
}
