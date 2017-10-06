defmodule ExDoc.Markdown.Hoedown do
  @moduledoc """
  ExDoc extension for the Hoedown MarkDown parser.
  """
  @behaviour ExDoc.MarkdownProcessor

  def assets(:html), do: []
  def assets(:epub), do: []
  
  def before_closing_head_tag(:html), do: ""
  def before_closing_head_tag(:epub), do: ""
  
  def before_closing_body_tag(:html), do: ""
  def before_closing_body_tag(:epub), do: ""

  @doc """
  Check if the Hoedown MarkDown parser module is available. Otherwise, try to
  load the module.
  """
  def available? do
    Code.ensure_loaded?(Markdown)
  end

  @doc """
  Hoedown specific options:

    * `:autolink` - defaults to true
    * `:fenced_code` - defaults to true
    * `:tables` - Enables Markdown Extra style tables, defaults to true

  """
  def to_html(text, opts) do
    options =
      [autolink: Keyword.get(opts, :autolink, true),
       fenced_code: Keyword.get(opts, :fenced_code, true),
       tables: Keyword.get(opts, :tables, true)]

    text
    |> Markdown.to_html(options)
    |> pretty_codeblocks()
  end

  @doc false
  # Helper to handle fenced code blocks (```...```) with
  # language specification
  defp pretty_codeblocks(bin) do
    # Hoedown parser puts the prefix "language-" as part of the class value
    bin = Regex.replace(~r/<pre><code\s+class=\"language-([^\"]+)\">/,
                        bin, ~S(<pre><code class="\1">))

    bin
  end
end
