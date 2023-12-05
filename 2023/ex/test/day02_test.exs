defmodule Day02Test do
  use ExUnit.Case
  doctest Day02

  test "parse line" do
    data = [
      {
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
        [%Cubes{red: 4, blue: 3, green: 0}, %Cubes{red: 1, green: 2, blue: 6}, %Cubes{green: 2}]
      }
    ]

    Enum.each(data, fn {input, expected} ->
      assert Day02.parse(input) == expected
    end)
  end

  test "combine turn results" do
    data = [
      {
        [{3, :blue}, {4, :red}, {1, :red}, {2, :green}, {6, :blue}, {2, :green}],
        %Cubes{red: 5, green: 4, blue: 9}
      }
    ]

    Enum.each(data, fn {input, expected} ->
      assert Cubes.from_raw_str(input) == expected
    end)
  end

  test "solve first part" do
    input = [
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]

    expected = 8

    assert Day02.solve(input) == expected
  end

  test "set power" do
    data = [
      {
        [%Cubes{red: 4, blue: 3}, %Cubes{red: 1, green: 2, blue: 6}, %Cubes{green: 2}],
        48
      }
    ]

    Enum.each(data, fn {input, expected} ->
      assert Cubes.power(input) == expected
    end)
  end
end
