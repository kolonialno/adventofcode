module Day06

function readdata(path)
    open(path) do file
        data = []
        group = []
        for line in eachline(file)
            if length(line) == 0
                push!(data, group)
                group = []
            else
                push!(group, Set(line))
            end
        end
        push!(data, group)
        data
    end
end

function part1(data)
    mapreduce(length, +, map(group -> reduce(union, group), data))
end

function part2(data)
    mapreduce(length, +, map(group -> reduce(intersect, group), data))
end

function main()
    data = readdata("input/input06.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2(data))
end

end # module
