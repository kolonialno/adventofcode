# Read input
lines = []
open("erik/input/input02.txt") do file
    for line in eachline(file)
        parts = match(r"(\d+)-(\d+) (.): (.*)", line).captures
        push!(lines, (min = parse(Int, parts[1]), max = parse(Int, parts[2]),
                    char = (parts[3])[1], password = parts[4]))
    end
end

# Part one
part1 = (filter(line -> line.min
                        <= length(filter(c -> c == line.char, line.password))
                        <= line.max,
                lines)
         |> length)

# Part two
part2 = (filter(line -> xor((line.password[line.min] == line.char),
                            (line.password[line.max] == line.char)),
                lines)
         |> length)

# Output results
println("> Part one: ", part1)
println("> Part two: ", part2)
