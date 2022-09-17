defmodule ExDoc.Formatter.HTML.Assets do
  @moduledoc false

  defmacrop embed_pattern(pattern) do
    ["formatters/html", pattern]
    |> Path.join()
    |> Path.wildcard()
    |> Enum.map(&{Path.basename(&1), File.read!(&1)})
  end

  def dist(proglang), do: dist_js() ++ dist_css(proglang) ++ dist_license()

  defp dist_js(), do: embed_pattern("dist/*.js")
  defp dist_css(:elixir), do: embed_pattern("dist/html-elixir-*.css")
  defp dist_css(:erlang), do: embed_pattern("dist/html-erlang-*.css")
  defp dist_license(), do: embed_pattern("dist/*.LICENSE.txt")

  def fonts, do: embed_pattern("dist/*.woff2")
end
