defmodule ExDoc.Markdown.Cmark do
  @moduledoc """
  ExDoc extension for the Cmark Markdown parser
  """
  @behaviour ExDoc.MarkdownProcessor

  def assets(:html), do: []
  def assets(:epub), do: []
  
  def before_closing_head_tag(:html), do: ""
  def before_closing_head_tag(:epub), do: ""
  
  def before_closing_body_tag(:html), do: ""
  def before_closing_body_tag(:epub), do: ""

  @doc """
  Check if the Cmark Markdown parser module is available. Otherwise, try to
  load the module
  """
  def available? do
    Code.ensure_loaded?(Cmark)
  end

  @doc """
  Generate HTML output. Cmark takes no options.
  """
  def to_html(text, _opts) do
    text |> Cmark.to_html()  |> ExDoc.Markdown.pretty_codeblocks()
  end
end
