module Day15

function elfnum(seq, k)
    hist = Dict{Int64, Int64}()
    for i in 1:(length(seq) - 1)
        hist[seq[i]] = i
    end
    next = last(seq)
    for i in length(seq):(k-1)
        pos = get(hist, next, 0)
        hist[next] = i
        next = pos == 0 ? 0 : i - pos
    end
    next
end

function part1(data)
    elfnum(data, 2020)
end

function part2(data)
    elfnum(data, 30000000)
end

function main()
    data = [14,8,16,0,1,17]
    println("> Part one: ", @time part1(data))
    println("> Part two: ", @time part2(data))
end

end # module
