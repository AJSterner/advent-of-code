def parse_directions(filename):
    with open(filename, encoding='locale') as f:
        return [(d, int(x)) for d, x in (line.split() for line in f)]

def part1(directions):
    h, v = (0, 0)
    for direction in directions:
        match direction:
            case ('forward', x):
                h, v = (h + x, v)
            case ('up', x):
                h, v = (h, v - x)
            case ('down', x):
                h, v = (h, v + x)
    return h * v

def part2(directions):
    h, v, aim = (0, 0, 0)
    for direction in directions:
        match direction:
            case ('forward', x):
                h, v, aim = (h + x, v + aim * x, aim)
            case ('up', x):
                h, v, aim = (h, v, aim - x)
            case ('down', x):
                h, v, aim = (h, v, aim + x)
    return h * v

if __name__ == '__main__':
    test_directions = parse_directions('test')
    directions = parse_directions('input')

    print(f'Part1 test: {part1(test_directions)}')
    print(f'Part1: {part1(directions)}')
    print(f'Part2 test: {part2(test_directions)}')
    print(f'Part2: {part2(directions)}')
