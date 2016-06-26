defmodule ExDoc.Markdown.Earmark do
  @moduledoc """
  ExDoc extension for the Earmark MarkDown parser
  """

  @doc """
  Check if the Earmark MarkDown parser module is available. Otherwise, try to
  load the module
  """
  def available? do
    Code.ensure_loaded?(Earmark)
  end

  @doc """
  Earmark specific options:

  * `gfm`

     True by default. Turns on Github Flavored Markdown extensions

  * `breaks`: boolean

    Only applicable if `gfm` is enabled. Makes all line breaks
    significant (so every line in the input is a new line in the
    output.

  * `smartypants`: boolean

    Turns on smartypants processing, so quotes become curly, two
    or three hyphens become en and em dashes, and so on. True by
    default.

  """
  def to_html(text, opts \\ []) do
    line = Keyword.get(opts, :line) || 0

    options = struct(Earmark.Options,
      gfm:         Keyword.get(opts, :gfm,         true),
      breaks:      Keyword.get(opts, :breaks,      false),
      smartypants: Keyword.get(opts, :smartypants, true),
      file:        Keyword.get(opts, :file),
      line:        line + 1)
    Earmark.to_html(text, options)
  end
end
