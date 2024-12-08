defmodule ExDoc.Formatter.Markdown.Assets do
  @moduledoc false

  defmacrop embed_pattern(pattern) do
    ["formatters/markdown", pattern]
    |> Path.join()
    |> Path.wildcard()
    |> Enum.map(fn path ->
      Module.put_attribute(__CALLER__.module, :external_resource, path)
      {Path.basename(path), File.read!(path)}
    end)
  end

  def dist(_proglang), do: []

  def metainfo, do: embed_pattern("metainfo/*")
end
