defmodule ExDoc.ExtraNode do
  @moduledoc """
  A structure representing an extra file (guide/documentation) to be included in generated docs.

  This separates the building and processing of extras from the individual formatters,
  allowing for better code reuse and cleaner architecture.
  """

  @type t :: %__MODULE__{
          id: String.t(),
          title: String.t(),
          title_content: String.t(),
          source: String.t(),
          source_path: String.t(),
          source_url: String.t() | nil,
          group: String.t() | nil,
          content: map()
        }

  @enforce_keys [:id, :title, :source, :source_path, :content]
  defstruct [
    :id,
    :title,
    :title_content,
    :source,
    :source_path,
    :source_url,
    :group,
    :content
  ]

  @doc """
  Builds extra nodes from configuration, processing them into a format-agnostic structure.

  The content field contains processed content for different formats:
  - `:ast` - The parsed AST from the source
  - `:html` - Rendered HTML content
  - `:markdown` - Rendered Markdown content
  - `:epub` - Rendered EPUB/XHTML content
  """
  def build_extras(config) do
    ExDoc.Formatter.build_extras_for_extra_node(config)
  end

  @doc """
  Gets the rendered content for a specific format.
  """
  def content_for_format(%__MODULE__{content: content}, format) do
    Map.get(content, format)
  end

  @doc """
  Checks if content exists for a specific format.
  """
  def has_format?(%__MODULE__{content: content}, format) do
    Map.has_key?(content, format)
  end
end