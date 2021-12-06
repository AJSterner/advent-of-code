import aoc
from day04.part1 import BingoBoard

def solution(puzzle_input: aoc.Input):
    numbers, *boards = puzzle_input.paragraphs()
    numbers = (int(n) for n in numbers.split(','))
    boards = [BingoBoard(board) for board in boards]
    for number in numbers:
        unfinished_boards = [board for board in boards if not board.mark(number)]
        if len(boards) == 1 and boards[0].mark(number):
            return number * boards[0].score()
        boards = unfinished_boards

def main():
    aoc.print_answers(solution, __file__)

if __name__ == '__main__':
    main()