defmodule ExDoc.Formatter.HTML.Assets do
  @moduledoc false

  @assets_dir Application.app_dir(:ex_doc, "priv/ex_doc/formatter/html/assets")

  defmacrop embed_pattern(pattern) do
    [@assets_dir, pattern]
    |> Path.join()
    |> Path.wildcard()
    |> Enum.map(&{Path.basename(&1), File.read!(&1)})
  end

  def debug do
    embed_pattern("dist/*.map")
  end

  def dist do
    embed_pattern("dist/*.{css,js}")
  end

  def fonts do
    embed_pattern("fonts/*")
  end
end
