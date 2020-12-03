defmodule Day3 do
  def solve(input, x_step, y_step) do
    solve(input, 0, 0, x_step, y_step)
  end

  def solve([current_row | rest], x_index, n_trees, x_step, y_step) do
    tree_here = if Enum.at(current_row, rem(x_index, length(current_row))) == "#", do: 1, else: 0

    remaining_rows =
      if y_step > 1 do
        rest = Enum.slice(rest, (y_step - 1)..-1)
      else
        rest
      end

    solve(remaining_rows, x_index + x_step, n_trees + tree_here, x_step, y_step)
  end

  def solve([], _, n_trees, _, _) do
    n_trees
  end
end

{:ok, file} = File.read("input/day3.txt")

input =
  file
  |> String.split("\n")
  |> Enum.map(&String.graphemes/1)

IO.puts("Task 1")

input
|> Day3.solve(3, 1)
|> IO.inspect()

IO.puts("Task 2")

[
  {1, 1},
  {3, 1},
  {5, 1},
  {7, 1},
  {1, 2}
]
|> Enum.map(fn {x_step, y_step} ->
  Day3.solve(input, x_step, y_step)
end)
|> Enum.reduce(&Kernel.*/2)
|> IO.inspect()
