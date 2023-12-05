defmodule Input do
  def load(path) do
    case File.read(path) do
      {:ok, content} ->
        String.split(content, "\n", trim: true)

      _ ->
        []
    end
  end
end
