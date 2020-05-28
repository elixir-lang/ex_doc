defmodule ExDoc.Formatter.EPUB.Assets do
  @moduledoc false

  defmacrop embed_pattern(pattern) do
    ["formatters/epub", pattern]
    |> Path.join()
    |> Path.wildcard()
    |> Enum.map(&{Path.basename(&1), File.read!(&1)})
  end

  def dist(proglang), do: dist_js() ++ dist_css(proglang)

  defp dist_js(), do: embed_pattern("dist/*.js")
  defp dist_css(:elixir), do: embed_pattern("dist/elixir-*.css")
  defp dist_css(:erlang), do: embed_pattern("dist/erlang-*.css")

  def metainfo do
    embed_pattern("metainfo/*")
  end
end
