defmodule Day4 do
  def is_valid?(value) do
    parsed_values = value |> String.split(~r/\s/) |> Enum.map(&String.slice(&1, 0..2))

    required_fields = [
      "byr",
      "iyr",
      "eyr",
      "hgt",
      "hcl",
      "ecl",
      "pid"
    ]

    Enum.all?(required_fields, &Enum.member?(parsed_values, &1))
  end
end

{:ok, file} = File.read("input/day4.txt")

file
|> String.split("\n\n")
|> Enum.filter(&Day4.is_valid?/1)
|> Enum.count()
|> IO.inspect()
