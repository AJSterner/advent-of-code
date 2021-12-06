import numpy as np
import aoc

class BingoBoard():
    def __init__(self, board_text):
        self.board = np.array([int(n) for n in board_text.split()]).reshape((5, 5))
        self.marked = np.full((5, 5), False)
        self.indeces = {n : idx for idx, n in np.ndenumerate(self.board)}
        assert(len(self.indeces) == 25)
    
    def mark(self, n):
        if not n in self.indeces:
            return False
        idx = self.indeces[n]
        self.marked[idx] = True
        return self.won(idx)
    
    def won(self, last_mark_idx):
        return all(self.marked[last_mark_idx[0]]) or all(self.marked[:, last_mark_idx[1]])
    
    def score(self):
        return self.board[~self.marked].sum()

def solution(puzzle_input: aoc.Input):
    numbers, *boards = puzzle_input.paragraphs()
    numbers = (int(n) for n in numbers.split(','))
    boards = [BingoBoard(board) for board in boards]
    for number in numbers:
        for board in boards:
            if board.mark(number):
                return number * board.score()

def main():
    aoc.print_answers(solution, __file__)

if __name__ == '__main__':
    main()