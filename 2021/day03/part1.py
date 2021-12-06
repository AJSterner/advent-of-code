import aoc
import numpy as np

def to_int(bs):
    x = 0
    for b in bs:
        x = (x << 1) + b
    return x

def solution(puzzle_input: aoc.Input):
    codes = np.array([[int(b) for b in line.strip()] for line in puzzle_input.lines()])
    gamma_arr = codes.sum(axis=0) > (len(codes) // 2)
    gamma = to_int(gamma_arr)
    epsilon = to_int(~gamma_arr)
    return gamma * epsilon

def main():
    aoc.print_answers(solution, __file__)
    
if __name__ == '__main__':
    main()