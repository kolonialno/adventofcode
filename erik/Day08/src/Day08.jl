module Day08

function readdata(path)
    open(path) do file
        data = []
        for line in eachline(file)
            s = split(line, " ")
            push!(data, (op=s[1], arg=parse(Int, s[2])))
        end
        data
    end
end

function step(state, code, flip)
    inst = code[state.line]
    if inst.op == "nop" || inst.op == "jmp" && flip
        return (line=state.line + 1, acc=state.acc)
    end
    if inst.op == "jmp" || inst.op == "nop" && flip
        return (line=state.line + inst.arg, acc=state.acc)
    end
    (line=state.line + 1, acc=state.acc + inst.arg)
end

function run(code, flipline)
    state = (line=1, acc=0)
    lines = length(code)
    visits = Set()
    while !(state.line in visits) && state.line <= lines
        push!(visits, state.line)
        state = step(state, code, state.line == flipline)
    end
    state
end

function part1(data)
    run(data, nothing).acc
end

function part2(data)
    for line in 1:length(data)
        state = run(data, line)
        if state.line > length(data)
            return state.acc
        end
    end
end

function main()
    data = readdata("input/input08.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2(data))
end

end # module
