defmodule ExDoc.Formatter.Markdown.Assets do
  @moduledoc false

  defmacrop embed_pattern(pattern) do
    ["formatters/markdown", pattern]
    |> Path.join()
    |> Path.wildcard()
    |> Enum.map(&{Path.basename(&1), File.read!(&1)})
  end

  def dist(_proglang), do: dist_license()

  defp dist_license(), do: embed_pattern("dist/*.LICENSE.txt")
end
