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

  test "play" do
    lines = [
      "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
      "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
      "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
      "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
      "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
      "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    ]

    expected_acc = %{1 => 1, 2 => 2, 3 => 4, 4 => 8, 5 => 14, 6 => 1}
    assert Day04.play(lines) == expected_acc
  end

  test "count cards" do
    cards = %{1 => 1, 2 => 2, 3 => 4, 4 => 8, 5 => 14, 6 => 1}
    expected_count = 30
    assert Day04.count_cards(cards) == expected_count
  end
end
