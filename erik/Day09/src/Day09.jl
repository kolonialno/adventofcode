module Day09

using DelimitedFiles

function readdata(path)
    readdlm(path, '\t', Int)
end

function validate(pos, data)
    k = data[pos]
    p = data[pos-25:pos-1]
    for m in p
        for n in p
            if k == m + n return true end
        end
    end
    return false
end

function part1(data)
    for i in 26:length(data)
        if !validate(i, data) return data[i] end
    end
end

function findrange(a, b, acc, target, data)
    acc == target && return minimum(data[a:b]) + maximum(data[a:b])
    acc < target ?
        findrange(a, b+1, acc+data[b+1], target, data) :
        findrange(a+1, b, acc-data[a], target, data)
end

function part2(data)
    findrange(1, 2, data[1] + data[2], part1(data), data)
end

function main()
    data = readdata("input/input09.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2b(data))
end


end # module