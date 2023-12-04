defmodule Day04 do
  def parse(line) do
    [_, raw_cards] = String.split(line, ":")
    [winning_cards, cards_in_possession] = raw_cards |> String.split(" | ")
    [winning_cards |> get_numbers(), cards_in_possession |> get_numbers()]
  end

  def matching_cards_count(winning_cards, cards) do
    MapSet.intersection(
      cards |> MapSet.new(),
      winning_cards |> MapSet.new()
    )
    |> MapSet.size()
  end

  #### Part 1
  def run1 do
    "./input/day04.txt"
    |> Input.load()
    |> solve1()
  end

  def solve1(lines) do
    Enum.reduce(
      lines,
      0,
      fn line, acc ->
        [winning_cards, cards] = parse(line)
        acc + get_score(winning_cards, cards)
      end
    )
  end

  def get_score(winning_cards, cards) do
    matching_cards_count(winning_cards, cards) |> len_to_score()
  end

  defp len_to_score(len) do
    if(len == 0) do
      0
    else
      Integer.pow(2, len - 1)
    end
  end

  defp get_numbers(s), do: String.split(s)

  #### Part 2

  def run2 do
    "./input/day04.txt"
    |> Input.load()
    |> solve2()
  end

  def solve2(lines) do
    lines |> play() |> count_cards()
  end

  def count_cards(cards), do: cards |> Enum.reduce(0, fn {_, val}, acc -> val + acc end)

  def play(lines) do
    lines
    |> Enum.with_index()
    |> Enum.reduce(
      init_acc(lines),
      fn {line, idx}, acc ->
        play(line, idx, acc)
      end
    )
  end

  def play(line, idx, acc) do
    [winning_cards, cards] = parse(line)
    matches = matching_cards_count(winning_cards, cards)

    if matches == 0 do
      acc
    else
      bonus = Map.get(acc, idx + 1, 1) - 1

      bump_acc(Enum.map(1..matches, fn x -> x + idx + 1 end), acc, bonus)
    end
  end

  def init_acc(lines) do
    lines
    |> Enum.with_index()
    |> Enum.reduce(
      %{},
      fn {_, idx}, acc -> Map.put(acc, idx + 1, 1) end
    )
  end

  def bump_acc([], acc, _), do: acc

  def bump_acc([head | tail], acc, bonus) do
    current = Map.get(acc, head)

    if current == nil do
      acc
    else
      inc = 1 + bonus
      val = current + inc
      bump_acc(tail, Map.put(acc, head, val), bonus)
    end
  end
end
