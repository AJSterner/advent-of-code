import aoc

def count_set_bits(codes: list[int], bit: int) -> int:
    return sum(bool(bit & code) for code in codes)

def rating(codes: list[int], bit: int, keep_majority: bool) -> list[int]:
    while len(codes) > 1:
        majority_set = count_set_bits(codes, bit) >= (len(codes) / 2)
        in_minority = lambda code: bool(code & bit) ^ majority_set
        codes = [code for code in codes if in_minority(code) ^ keep_majority]
        bit >>= 1
    return codes[0]

def solution(puzzle_input: aoc.Input):
    lines = puzzle_input.lines()
    codes = [int(line, base=2) for line in lines]
    code_width = len(lines[0].strip())
    msb = 1 << (code_width - 1)
    oxygen_generator_rating = rating(codes, msb, True)
    CO2_scrubber_rating = rating(codes, msb, False)
    return oxygen_generator_rating * CO2_scrubber_rating

def main():
    aoc.print_answers(solution, __file__)

if __name__ == '__main__':
    main()