module Day05

function readdata(path)
    open(path) do file
        data = []
        for line in eachline(file)
            push!(data, parse(Int,
                              map(c -> Dict('F' => '0', 'B' => '1',
                                            'L' => '0', 'R' => '1')[c],
                                  line),
                              base=2))
        end
        data
    end
end

function part1(data)
    maximum(data)
end

function part2(data)
    for i in 1:1022
        if i ∉ data && i-1 ∈ data && i+1 ∈ data
            return i
        end
    end
end

function main()
    data = readdata("input/input05.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2(data))
end

end # module
