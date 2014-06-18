defmodule ExDoc.Markdown.Hoedown do
  def available? do
    Code.ensure_loaded?(Markdown)
  end

  @doc """
  Hoedown specific options:

    * `:autolink` - defaults to true
    * `:tables` - Enables Markdown Extra style tables, defaults to true

  """
  def to_html(text, opts \\ []) do
    Markdown.to_html(text,
      autolink: Keyword.get(opts, :autolink, true),
      tables: Keyword.get(opts, :tables, true))
  end
end
