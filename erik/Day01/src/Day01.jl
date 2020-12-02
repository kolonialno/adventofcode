module Day01

using DelimitedFiles

function readdata(path)
    readdlm(path, '\t', Int)
end

function part1(numbers)
    reduce(*, filter(n -> in(2020 - n, numbers), numbers))
end

function part2(numbers)
    for m in numbers
        pair = filter(n -> in(2020 - m - n, numbers), numbers)
        if length(pair) == 2
            return m * pair[1] * pair[2]
        end
    end
end

function main()
    data = readdata("input/input01.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2(data))
end

end # module
