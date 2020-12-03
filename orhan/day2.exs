{:ok, file} = File.read("input/day2.txt")

defmodule Day2 do
  def parse_rule(rule) do
    %{
      "from" => from,
      "to" => to,
      "letter" => letter
    } = Regex.named_captures(~r/(?<from>\d+)-(?<to>\d+) (?<letter>[a-z])/, rule)

    {from, _} = Integer.parse(from)
    {to, _} = Integer.parse(to)
    {from, to, letter}
  end

  def is_valid?({rule, password}, :old) do
    {from, to, letter} = parse_rule(rule)

    length =
      password
      |> String.codepoints()
      |> Enum.filter(&(letter == &1))
      |> Enum.count()

    from <= length and length <= to
  end

  def is_valid?({rule, password}, :new) do
    {from, to, letter} = parse_rule(rule)

    codepoints = String.codepoints(password)

    (Enum.at(codepoints, from - 1) == letter and Enum.at(codepoints, to - 1) != letter) or
      (Enum.at(codepoints, from - 1) != letter and Enum.at(codepoints, to - 1) == letter)
  end
end

file
|> String.split("\n")
|> Enum.map(&String.split(&1, ": "))
|> Enum.map(&List.to_tuple/1)
|> Enum.filter(&Day2.is_valid?(&1, :old))
|> Enum.count()
|> IO.inspect()

file
|> String.split("\n")
|> Enum.map(&String.split(&1, ": "))
|> Enum.map(&List.to_tuple/1)
|> Enum.filter(&Day2.is_valid?(&1, :new))
|> Enum.count()
|> IO.inspect()
