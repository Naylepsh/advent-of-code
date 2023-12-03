defmodule Day01 do
  def run1, do: solve(fn x -> x end)
  def run2, do: solve(&prepare/1)

  def solve(prepare) do
    "./input/day01.txt"
    |> Input.load()
    |> solve(prepare)
  end

  def solve(lines, prepare) do
    lines
    |> Enum.map(&prepare.(&1))
    |> Enum.map(&calibration_value(&1))
    |> Enum.sum()
  end

  def calibration_value(line) do
    chars = String.split(line, "")
    first = chars |> Enum.find_value(&digit(&1))
    last = chars |> Enum.reverse() |> Enum.find_value(&digit(&1))

    first * 10 + last
  end

  def digit(x) do
    maybe_int = Integer.parse(x)

    if maybe_int != :error do
      {value, _} = maybe_int
      value
    end
  end

  def prepare(text), do: prepare(text, "")
  def prepare("", acc), do: String.reverse(acc)
  def prepare("one" <> rest, acc), do: prepare("ne" <> rest, "1" <> acc)
  def prepare("two" <> rest, acc), do: prepare("wo" <> rest, "2" <> acc)
  def prepare("three" <> rest, acc), do: prepare("hree" <> rest, "3" <> acc)
  def prepare("four" <> rest, acc), do: prepare("our" <> rest, "4" <> acc)
  def prepare("five" <> rest, acc), do: prepare("ive" <> rest, "5" <> acc)
  def prepare("six" <> rest, acc), do: prepare("ix" <> rest, "6" <> acc)
  def prepare("seven" <> rest, acc), do: prepare("even" <> rest, "7" <> acc)
  def prepare("eight" <> rest, acc), do: prepare("ight" <> rest, "8" <> acc)
  def prepare("nine" <> rest, acc), do: prepare("ine" <> rest, "9" <> acc)
  def prepare("1" <> rest, acc), do: prepare(rest, "1" <> acc)
  def prepare("2" <> rest, acc), do: prepare(rest, "2" <> acc)
  def prepare("3" <> rest, acc), do: prepare(rest, "3" <> acc)
  def prepare("4" <> rest, acc), do: prepare(rest, "4" <> acc)
  def prepare("5" <> rest, acc), do: prepare(rest, "5" <> acc)
  def prepare("6" <> rest, acc), do: prepare(rest, "6" <> acc)
  def prepare("7" <> rest, acc), do: prepare(rest, "7" <> acc)
  def prepare("8" <> rest, acc), do: prepare(rest, "8" <> acc)
  def prepare("9" <> rest, acc), do: prepare(rest, "9" <> acc)
  def prepare(other, acc), do: prepare(String.slice(other, 1..-1), acc)
end
