defmodule Day1a do

    def result(num) do
        num * (2020-num)
    end

    def solve([], _map) do
        IO.puts "No solution found"
    end

    def solve([num|rest], map) do
        if Map.has_key?(map, 2020-num) do
            result(num)
        else
            solve(rest, Map.put(map, num, 1))
        end
    end

    def solve(input) do
        solve(input, %{})
    end
end

defmodule Day1b do
    def solve(input) do
        for i <- input, j <- input, k <- input, i+j+k == 2020, uniq: true, do: i*j*k
    end
end

{:ok, file} = File.read("input/day1.txt")

parsed = file
|> String.split("\n")
|> Enum.map(fn val -> elem(Integer.parse(val), 0) end)

parsed
|> Day1a.solve
|> IO.inspect

parsed
|> Day1b.solve
|> IO.inspect
