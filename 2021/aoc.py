""" AOC helpers """
from typing import Callable, Any
from pathlib import Path

class Input:
    """
    provides various helpers to parse and manipulate AOC input
    """
    def __init__(self, path):
        self.path = path

    def open_file(self):
        """ returns open file """
        return open(self.path, encoding='locale')

    def lines(self):
        """ splits input on new lines """
        with self.open_file() as file:
            return file.readlines()

    def paragraphs(self):
        """ splits input on blank lines """
        with self.open_file() as file:
            return file.read().split('\n\n')

def day_directory(script_path):
    """ call this with __file__ """
    return Path(script_path).resolve().parent

def test_input(script_path):
    """ call this with __file__ """
    return Input(day_directory(script_path) / 'test')

def puzzle_input(script_path):
    """ call this with __file__ """
    return Input(day_directory(script_path) / 'input')

def print_answers(solution: Callable[[Input], Any], script_path: str | Path, test_only: bool=False):
    """ call this with __file__ as script_path """
    print(f'Test answer: {solution(test_input(script_path))}')
    if not test_only:
        print(f'Puzzle answer: {solution(puzzle_input(script_path))}')
