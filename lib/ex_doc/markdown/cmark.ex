defmodule ExDoc.Markdown.Cmark do
  @moduledoc """
  ExDoc extension for the Cmark Markdown parser.
  """
  @behaviour ExDoc.Markdown

  @doc """
  Check if the Cmark Markdown parser module is available.
  """
  def available? do
    Code.ensure_loaded?(Cmark)
  end

  @doc """
  Generate HTML output.

  ## Options

    * `:highlight_tag` - the tag used for highlighting code, defaults to "span"
  """
  def to_html(text, opts) do
    text
    |> Cmark.to_html()
    |> ExDoc.Highlighter.highlight_code_blocks(opts)
  end
end
