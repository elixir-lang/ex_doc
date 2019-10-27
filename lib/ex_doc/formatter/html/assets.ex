defmodule ExDoc.Formatter.HTML.Assets do
  @moduledoc false

  defmacrop embed_pattern(pattern) do
    ["formatters/html", pattern]
    |> Path.join()
    |> Path.wildcard()
    |> Enum.map(&{Path.basename(&1), File.read!(&1)})
  end

  def dist do
    embed_pattern("dist/*.{css,js}")
  end

  def fonts do
    embed_pattern("fonts/*")
  end
end
