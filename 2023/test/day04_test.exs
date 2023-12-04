defmodule Day04Test do
  use ExUnit.Case
  doctest Day04

  test "get score" do
    data = [
      {
        ["41", "48", "83", "86", "17"],
        ["83", "86", "6", "31", "17", "9", "48", "53"],
        8
      }
    ]

    Enum.each(data, fn {winning_cards, cards, expected_score} ->
      assert Day04.get_score(winning_cards, cards) == expected_score
    end)
  end
end
