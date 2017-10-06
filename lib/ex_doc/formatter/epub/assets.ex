defmodule ExDoc.Formatter.EPUB.Assets do
  @moduledoc false

  defmacrop embed_pattern(pattern) do
    ["formatters/epub", pattern]
    |> Path.join()
    |> Path.wildcard()
    |> Enum.map(&{Path.basename(&1), File.read!(&1)})
  end

  def dist do
    embed_pattern("dist/*")
  end

  def metainfo do
    embed_pattern("metainfo/*")
  end

  def markdown_processor_assets do
    ExDoc.Markdown.get_markdown_processor().assets(:epub)
  end
end
