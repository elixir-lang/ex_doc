defmodule ExDoc.Markdown.Cmark do
  @moduledoc """
  ExDoc extension for the Cmark Markdown parser.
  """
  @behaviour ExDoc.Markdown

  # Callback implementations
  def assets(arg), do: ExDoc.Highlighter.assets(arg)

  def before_closing_head_tag(arg), do: ExDoc.Highlighter.before_closing_head_tag(arg)

  def before_closing_body_tag(arg), do: ExDoc.Highlighter.before_closing_body_tag(arg)

  def configure(_), do: :ok

  @doc """
  Check if the Cmark Markdown parser module is available.
  """
  def available? do
    Code.ensure_loaded?(Cmark)
  end

  @doc """
  Generate HTML output. Cmark takes no options.
  """
  def to_html(text, _opts) do
    text
    |> Cmark.to_html()
    |> ExDoc.Highlighter.highlight_code_blocks()
  end
end
