defmodule Cubes do
  defstruct red: 0, green: 0, blue: 0

  def from_raw_str(str), do: List.foldl(str, %Cubes{}, &add/2)

  defp add({v, :red}, cubes), do: %{cubes | red: cubes.red + v}
  defp add({v, :green}, cubes), do: %{cubes | green: cubes.green + v}
  defp add({v, :blue}, cubes), do: %{cubes | blue: cubes.blue + v}

  defp is_possible?(cubes, max_cubes) do
    cubes.red <= max_cubes.red and cubes.green <= max_cubes.green and cubes.blue <= max_cubes.blue
  end

  def are_possible?(cubess, max_cubes),
    do: Enum.all?(cubess, &is_possible?(&1, max_cubes))

  defp min_needed(cubess), do: min_needed(cubess, %Cubes{})
  defp min_needed([], acc), do: acc

  defp min_needed([head | tail], acc) do
    min_needed(tail, %Cubes{
      red: Enum.max([head.red, acc.red]),
      green: Enum.max([head.green, acc.green]),
      blue: Enum.max([head.blue, acc.blue])
    })
  end

  def power([head | tail]) do
    [head | tail] |> min_needed() |> power()
  end

  def power(cube), do: cube.red * cube.green * cube.blue
end

defmodule Day02 do
  def run1 do
    "./input/day02.txt"
    |> Input.load()
    |> solve()
  end

  def run2 do
    "./input/day02.txt"
    |> Input.load()
    |> solve2()
  end

  def solve(lines) do
    lines
    |> Enum.map(&parse/1)
    |> Enum.with_index()
    |> Enum.filter(fn {results, _} ->
      Cubes.are_possible?(results, %Cubes{red: 12, green: 13, blue: 14})
    end)
    |> Enum.map(fn {_, index} -> index + 1 end)
    |> Enum.sum()
  end

  def solve2(lines) do
    lines
    |> Enum.map(&parse/1)
    |> Enum.map(&Cubes.power/1)
    |> Enum.sum()
  end

  def parse(line) do
    [_, raw_results] = String.split(line, ": ")

    raw_results
    |> String.split("; ")
    |> Enum.map(fn raw_result ->
      Regex.scan(~r/(\d+ (blue|red|green))/, raw_result)
      |> Enum.map(fn results ->
        [val, color] = results |> List.first() |> String.split(" ")
        {String.to_integer(val), parse_color(color)}
      end)
      |> Cubes.from_raw_str()
    end)
  end

  defp parse_color("red"), do: :red
  defp parse_color("green"), do: :green
  defp parse_color("blue"), do: :blue
end
