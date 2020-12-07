module Day07

function parsebag(spec)
    m = match(r"(\d+) (.*) bag", spec)
    if isnothing(m) return nothing end
    m[2] => parse(Int, m[1])
end

function readdata(path)
    open(path) do file
        data = Dict()   
        for line in eachline(file)
            m = match(r"^(.*?) bags contain(.*)", line)
            push!(data, m[1] => Dict(filter(!isnothing, map(parsebag, split(m[2], ",")))))
        end
        data
    end
end

function ancestors(child, data)
    parents = [bag for (bag, spec) in data if child in keys(spec)]
    if isempty(parents) return [] end
    union(parents, mapreduce(p -> ancestors(p, data), union, parents))
end

function numchildren(parent, data)
    if isempty(data[parent]) return 0 end
    reduce(+, map(((k, v),) -> v + v * numchildren(k, data), collect(data[parent])))
end

function part1(data)
    ancestors("shiny gold", data) |> length
end

function part2(data)
    numchildren("shiny gold", data)
end

function main()
    data = readdata("input/input07.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2(data))
end

end # module
