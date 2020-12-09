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

function part2(data)
    total = part1(data)
    for m in 1:length(data)
        for n in m+1:length(data)
            partial = sum(data[m:n])
            if partial > total
                continue
            end
            if partial == total
                return minimum(data[m:n]) + maximum(data[m:n])
            end
        end
    end
end

function main()
    data = readdata("input/input09.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2(data))
end

end # module
