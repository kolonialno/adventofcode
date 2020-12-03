module Day04

function readdata(path)
    open(path) do file
        data = []
        for line in eachline(file)
            push!(data, Nothing)
        end
        data
    end
end

function part1(data)
    Nothing
end

function part2(data)
    Nothing
end

function main()
    data = readdata("input/input04.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2(data))
end

end # module
