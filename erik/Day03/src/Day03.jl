module Day03

function readdata(path)
    open(path) do file
        data = []
        for line in eachline(file)
            push!(data, [Dict('.' => 0, '#' => 1)[c] for c in line])
        end
        data
    end
end

function counttrees(forest, right, down)
    patterwidth = length(forest[1])
    mapreduce(((i, row),) -> row[mod1(i*right - right + 1, patterwidth)],
              +,
              enumerate(data[1:down:end]))
end

function part1(data)
    counttrees(data, 3, 1)
end

function part2(data)
    mapreduce(((right, down),) -> counttrees(data, right, down),
              *,
              [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)])
end

function main()
    data = readdata("input/input03.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2(data))
end

end # module
