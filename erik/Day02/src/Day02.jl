module Day02

function readdata(path)
    open(path) do file
        data = []
        for line in eachline(file)
            parts = match(r"(\d+)-(\d+) (.): (.*)", line).captures
            push!(data, (min = parse(Int, parts[1]), max = parse(Int, parts[2]),
                         char = (parts[3])[1], password = parts[4]))
        end
        data
    end
end

function part1(data)
    (filter(line -> line.min
                    <= length(filter(c -> c == line.char, line.password))
                    <= line.max,
            data)
        |> length)
end

function part2(data)
    (filter(line -> xor((line.password[line.min] == line.char),
                        (line.password[line.max] == line.char)),
            data)
        |> length)
end

function main()
    data = readdata("input/input02.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2(data))
end

end # module
