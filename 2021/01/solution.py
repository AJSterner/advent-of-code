from itertools import tee
with open('input', encoding='locale') as f:
    depths = [int(line) for line in f]

def count_increasing(seq):
    x, y = tee(seq)
    next(y, None)
    return sum(a < b for a, b in zip(x, y))

part1 = count_increasing(depths)
print(f'Part1: {part1}')

part2 = count_increasing(sum(depths[i:i+3]) for i in range(len(depths) - 2))
print(f'Part2: {part2}')
