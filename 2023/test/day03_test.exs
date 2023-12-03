defmodule Day03Test do
  use ExUnit.Case
  doctest Day03

  test "get coords of symbols" do
    data = [
      {
        [
          "467..114..",
          "...*......",
          "..35..633.",
          "......#...",
          "617*......",
          ".....+.58.",
          "..592.....",
          "......755.",
          "...$.*....",
          ".664.598.."
        ],
        [
          {"*", 1, 3},
          {"#", 3, 6},
          {"*", 4, 3},
          {"+", 5, 5},
          {"$", 8, 3},
          {"*", 8, 5}
        ]
      }
    ]

    Enum.each(data, fn {input, expected} ->
      assert Day03.get_symbol_coords(input) == expected
    end)
  end

  test "get columns of numbers" do
    data = [
      {
        "467..114..",
        [{467, 0, 2}, {114, 5, 7}]
      },
      {
        "...",
        []
      }
    ]

    Enum.each(data, fn {input, expected} ->
      assert Day03.get_number_columns(input) == expected
    end)
  end

  test "get coords of numbers" do
    data = [
      {
        [
          "467..114..",
          "...*......",
          "..35..633."
        ],
        [
          {467, {0, 0}, {0, 2}},
          {114, {0, 5}, {0, 7}},
          {35, {2, 2}, {2, 3}},
          {633, {2, 6}, {2, 8}}
        ]
      }
    ]

    Enum.each(data, fn {input, expected} ->
      assert Day03.get_number_coords(input) == expected
    end)
  end

  test "adjacent? for number coords" do
    data = [
      {
        # .....
        # .42..
        # .....
        {42, {1, 1}, {1, 2}},
        [],
        false
      },
      {
        # .....
        # *42..
        # .....
        {42, {1, 1}, {1, 2}},
        [{"*", 1, 0}],
        true
      },
      {
        # .....
        # .42*.
        # .....
        {42, {1, 1}, {1, 2}},
        [{"*", 1, 3}],
        true
      },
      {
        # .*...
        # .42..
        # .....
        {42, {1, 1}, {1, 2}},
        [{"*", 0, 1}],
        true
      },
      {
        # *....
        # .42..
        # .....
        {42, {1, 1}, {1, 2}},
        [{"*", 0, 0}],
        true
      },
      {
        # .....
        # .42..
        # ....*
        {42, {1, 1}, {1, 2}},
        [{"*", 2, 4}],
        false
      },
      {
        # .....
        # .42..
        # +...*
        {42, {1, 1}, {1, 2}},
        [{"*", 2, 4}, {"+", 2, 0}],
        true
      }
    ]

    Enum.each(data, fn {number_coords, symbols_coords, expected} ->
      assert Day03.adjacent?(number_coords, symbols_coords) == expected
    end)
  end

  test "solve1" do
    input = [
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    ]

    expected = 4361

    assert Day03.solve1(input) == expected
  end

  test "solve2" do
    input = [
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    ]

    expected = 467_835

    assert Day03.solve2(input) == expected
  end
end
