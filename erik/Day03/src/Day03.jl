module Day03

function readdata(path)
    open(path) do file
        data = []
        for line in eachline(file)
            push!(data, [x == '#' ? 1 : 0 for x in line])
        end
        data
    end
end

function counttrees(data, x, y)
    patterwidth = length(data[1])
    mapreduce(((i, row),) -> row[mod((i - 1) * x, patterwidth) + 1],
              +,
              enumerate(data[1:y:end]))
end

function part1(data)
    counttrees(data, 3, 1)
end

function part2(data)
    reduce(*, map((x, y) -> counttrees(data, x, y), [1, 3, 5, 7, 1], [1, 1, 1, 1, 2]))
end

function main()
    data = readdata("input/input03.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2(data))
end

end # module

main()