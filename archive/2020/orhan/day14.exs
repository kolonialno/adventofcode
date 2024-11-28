defmodule Day14a do
  def parse_line(line) do
    if String.starts_with?(line, "mask =") do
      {:mask, String.slice(line, 7..-1)}
    else
      %{
        "address" => address,
        "value" => value
      } = Regex.named_captures(~r/mem\[(?<address>\d+)\] = (?<value>\d+)/, line)

      {address, _} = Integer.parse(address)
      {value, _} = Integer.parse(value)

      {:mem, address, value}
    end
  end

  def run_instructions([], memory, _), do: memory

  def run_instructions([instruction | rest], memory, mask) do
    {memory, mask} = execute_instruction(instruction, memory, mask)
    run_instructions(rest, memory, mask)
  end

  def apply_mask(value, mask) do
    # Ensure that value is 36 bits long
    v = <<value::size(36)>>

    bits = for <<x::1 <- v>>, do: <<x::1>>

    with_mask =
      Enum.with_index(bits)
      |> Enum.map(fn {bit, idx} ->
        if String.at(mask, idx) != "X",
          do: <<String.to_integer(String.at(mask, idx))::1>>,
          else: bit
      end)
      |> Enum.into(<<>>)

    # Pad with zeros for decode_unsigned
    <<0::4, with_mask::bitstring>>
    |> :binary.decode_unsigned()
  end

  def execute_instruction({:mask, mask}, memory, _) do
    {memory, mask}
  end

  def execute_instruction({:mem, address, value}, memory, mask) do
    {Map.put(memory, address, apply_mask(value, mask)), mask}
  end

  def solve(filename) do
    {:ok, file} = File.read(filename)

    file
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
    |> run_instructions(%{}, nil)
    |> Map.values()
    |> Enum.reduce(&Kernel.+/2)
  end
end

defmodule Day14b do
  def run_instructions([], memory, _), do: memory

  def run_instructions([instruction | rest], memory, mask) do
    {memory, mask} = execute_instruction(instruction, memory, mask)
    run_instructions(rest, memory, mask)
  end

  def possible_values(address, {letter, idx}) do
    case letter do
      "X" -> [<<0::1>>, <<1::1>>]
      "0" -> [Enum.at(address, idx)]
      "1" -> [<<1::1>>]
    end
  end

  def get_addresses(address, mask) do
    # Ensure that address is 36 bits long
    a = <<address::size(36)>>
    bits = for <<x::1 <- a>>, do: <<x::1>>

    mask
    |> String.graphemes()
    |> Enum.with_index()
    |> Enum.map(&possible_values(bits, &1))
    |> combine()
    |> Enum.map(&:binary.decode_unsigned(<<0::4, &1::bitstring>>))
  end

  def combine([curr | rest]) do
    tail = combine(rest)
    Enum.flat_map(curr, &Enum.map(tail, fn t -> <<&1::bitstring, t::bitstring>> end))
  end

  def combine([]) do
    [<<>>]
  end

  def execute_instruction({:mask, mask}, memory, _) do
    {memory, mask}
  end

  def execute_instruction({:mem, address, value}, memory, mask) do
    addresses = get_addresses(address, mask)

    new_memory =
      Enum.reduce(
        addresses,
        memory,
        &Map.put(&2, &1, value)
      )

    {new_memory, mask}
  end

  def solve(filename) do
    {:ok, file} = File.read(filename)

    file
    |> String.split("\n")
    # Reuse parsing
    |> Enum.map(&Day14a.parse_line/1)
    |> run_instructions(%{}, nil)
    |> Map.values()
    |> Enum.reduce(&Kernel.+/2)
  end
end

Day14a.solve("input/day14.txt") |> IO.inspect()

Day14b.solve("input/day14.txt") |> IO.inspect()
