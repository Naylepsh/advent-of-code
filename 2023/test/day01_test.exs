defmodule Day01Test do
  use ExUnit.Case
  doctest Day01

  test "calibrate value" do
    data = [
      {"1abc2", 12},
      {"pqr3stu8vwx", 38},
      {"a1b2c3d4e5f", 15},
      {"treb7uchet", 77}
    ]

    Enum.each(data, fn {input, expected} ->
      assert Day01.calibration_value(input) == expected
    end)
  end

  test "prepare line" do
    data = [
      {"123", "123"},
      {"one2three", "123"},
      {"1foo2bar", "12"},
      {"twone", "21"}
    ]

    Enum.each(data, fn {input, expected} ->
      assert Day01.prepare(input) == expected
    end)
  end

  test "first half example" do
    lines = [
      "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    ]

    assert Day01.solve(lines, fn x -> x end) == 142
  end
end
