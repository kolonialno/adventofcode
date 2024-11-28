defmodule Day10 do
  def solve_a(filename) do
    {:ok, file} = File.read(filename)

    adapters =
      file
      |> String.split("\n")
      |> Enum.map(&Integer.parse/1)
      |> Enum.map(&elem(&1, 0))

    # Wall socket
    adapters = [0 | adapters]
    # Device
    adapters = [Enum.max(adapters) + 3 | adapters]

    differences =
      adapters
      |> Enum.sort()
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.map(fn [a, b] -> b - a end)
      |> Enum.reduce(%{}, fn diff, acc -> Map.update(acc, diff, 1, &(&1 + 1)) end)

    Map.get(differences, 1, 0) * Map.get(differences, 3, 0)
  end

  def solve_b(filename) do
    {:ok, file} = File.read(filename)

    adapters =
      file
      |> String.split("\n")
      |> Enum.map(&Integer.parse/1)
      |> Enum.map(&elem(&1, 0))

    # Wall socket
    adapters = [0 | adapters]
    # Device
    adapters = [Enum.max(adapters) + 3 | adapters]

    single_skips =
      adapters
      |> Enum.sort()
      |> Enum.chunk_every(3, 1, :discard)
      |> Enum.map(&can_skip/1)
      |> Enum.reduce(&Kernel.+/2)
      |> IO.inspect()

    double_skips =
      adapters
      |> Enum.sort()
      |> Enum.chunk_every(4, 1, :discard)
      |> Enum.map(&can_skip/1)
      |> Enum.reduce(&Kernel.+/2)
      |> IO.inspect()

    IO.inspect("Skips")
    IO.inspect(single_skips)
    IO.inspect(double_skips)

    skips = single_skips + double_skips

    :math.pow(2, skips)
  end

  def can_skip(list) do
    first = Enum.at(list, 0)
    last = Enum.at(list, -1)
    if last - first <= 3, do: 1, else: 0
  end
end

Day10.solve_a("input/day10.txt") |> IO.inspect()

Day10.solve_b("input/day10.txt") |> IO.inspect()
