defmodule ExDoc.Formatter.EPUB.Assets do
  @moduledoc false

  @assets_dir Application.app_dir(:ex_doc, "priv/ex_doc/formatter/epub/assets")

  defmacrop embed_dir(dir) do
    [@assets_dir, dir, "*"]
    |> Path.join()
    |> Path.wildcard()
    |> Enum.map(&{Path.basename(&1), File.read!(&1)})
  end

  def dist do
    embed_dir("dist")
  end

  def metainfo do
    embed_dir("metainfo")
  end
end
