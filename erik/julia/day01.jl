using DelimitedFiles

# Read input
numbers = readdlm("erik/input/input01.txt", '\t', Int)

# Part one
part1 = reduce(*, filter(n -> in(2020 - n, numbers), numbers))

# Part two
part2 = 0

# Output results
println("> Part one: ", part1)
println("> Part two: ", part2)
