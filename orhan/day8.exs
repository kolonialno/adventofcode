defmodule Day8 do
  def solve(input) do
    virtual_machine(input, {0, 0, []})
  end

  def virtual_machine(code, {pc, acc, hist}) do
    if Enum.member?(hist, pc) do
      acc
    else
      if pc >= length(code) do
        {:error, :program_ended}
      else
        {new_pc, new_acc} = run_instruction(Enum.at(code, pc), pc, acc)
        virtual_machine(code, {new_pc, new_acc, [pc | hist]})
      end
    end
  end

  def run_instruction({"nop", _}, pc, acc) do
    {pc + 1, acc}
  end

  def run_instruction({"acc", input}, pc, acc) do
    {incr, _} = input |> String.trim() |> Integer.parse()
    {pc + 1, acc + incr}
  end

  def run_instruction({"jmp", input}, pc, acc) do
    {incr, _} = input |> String.trim() |> Integer.parse()
    {pc + incr, acc}
  end
end

{:ok, file} = File.read("input/day8.txt")

file
|> String.split("\n")
|> Enum.map(&String.split_at(&1, 3))
|> Day8.solve()
|> IO.inspect()
