defmodule Day04 do
  def run1 do
    "./input/day04.txt"
    |> Input.load()
    |> solve()
  end

  def solve(lines) do
    Enum.reduce(
      lines,
      0,
      fn line, acc ->
        [winning_cards, cards] = parse(line)
        acc + get_score(winning_cards, cards)
      end
    )
  end

  def parse(line) do
    [_, raw_cards] = String.split(line, ":")
    [winning_cards, cards_in_possession] = raw_cards |> String.split(" | ")
    [winning_cards |> get_numbers(), cards_in_possession |> get_numbers()]
  end

  def get_score(winning_cards, cards) do
    MapSet.intersection(
      cards |> MapSet.new(),
      winning_cards |> MapSet.new()
    )
    |> MapSet.size()
    |> len_to_score()
  end

  defp len_to_score(len) do
    if(len == 0) do
      0
    else
      Integer.pow(2, len - 1)
    end
  end

  defp get_numbers(s), do: String.split(s)
end
