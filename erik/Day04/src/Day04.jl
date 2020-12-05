module Day04

function readdata(path)
    open(path) do file
        data = []
        passport = Dict()
        for line in eachline(file)
            if length(line) == 0
                push!(data, passport)
                passport = Dict()
            else
                for field in split(line, " ")
                    key, value = split(field, ":")
                    push!(passport, key => value)
                end
            end
        end
        push!(data, passport)
        data
    end
end

function validate(passport)
    issubset(["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"], keys(passport))
end

function validateyear(passport, key, min, max)
    m = match(r"^(\d{4})$", passport[key])
    !isnothing(m) && (min <= parse(Int, m[1]) <= max)
end

function validatehgt(passport)
    m = match(r"^(\d+)(cm|in)$", passport["hgt"])
    (!isnothing(m)
     && (m[2] == "cm" && 150 <= parse(Int, m[1]) <= 193
         || m[2] == "in" && 59 <= parse(Int, m[1]) <= 76))
end

function validatehcl(passport)
    !isnothing(match(r"^#[0-9a-f]{6}$", passport["hcl"]))
end

function validateecl(passport)
    !isnothing(match(r"^(amb|blu|brn|gry|grn|hzl|oth)$", passport["ecl"]))
end

function validatepid(passport)
    !isnothing(match(r"^\d{9}$", passport["pid"]))
end


function validatefields(passport)
    (validate(passport)
     && validateyear(passport, "byr", 1920, 2002)
     && validateyear(passport, "iyr", 2010, 2020)
     && validateyear(passport, "eyr", 2020, 2030)
     && validatehgt(passport)
     && validatehcl(passport)
     && validateecl(passport)
     && validatepid(passport))
end

function part1(data)
    filter(validate, data) |> length
end

function part2(data)
    filter(validatefields, data) |> length
end

function main()
    data = readdata("input/input04.txt")
    println("> Part one: ", part1(data))
    println("> Part two: ", part2(data))
end

end # module
