fn main() raises:
    with open("input.txt", "r") as f:
        var input = f.read()
        var lines = input.splitlines()

        var first = List[Int]()
        var second = List[Int]()

        for idx in range(len(lines)):
            var elems = lines[idx].split()
            first.append(int(elems[0]))
            second.append(int(elems[1]))

        sort(first)
        sort(second)

        print("Part 1: " + str(part1(first, second)))
        print("Part 2: " + str(part2(first, second)))


fn part1(first: List[Int], second: List[Int]) -> Int:
    var sum = 0
    for idx in range(len(first)):
        sum += abs(first[idx] - second[idx])

    return sum


fn part2(first: List[Int], second: List[Int]) -> Int64:
    var i = 0
    var j = 0
    var score: Int64 = 0

    while i < len(first):
        var v = first[i]
        var count = 0
        while j < len(second) and second[j] < v:
            j += 1
        while j < len(second) and second[j] == v:
            count += 1
            j += 1
        while i < len(first) and first[i] == v:
            score += v * count
            i += 1

    return score
