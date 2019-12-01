defmodule ExDoc.Markdown.Earmark do
  @moduledoc """
  ExDoc extension for the Earmark MarkDown parser.
  """
  @behaviour ExDoc.Markdown

  @doc """
  Check if the Earmark Markdown parser module is available.
  """
  def available? do
    match?({:ok, _}, Application.ensure_all_started(:earmark)) and Code.ensure_loaded?(Earmark)
  end

  @doc """
  Generate HTML output.

  ## Options

    * `:gfm` - boolean. Turns on Github Flavored Markdown extensions. True by default

    * `:breaks` - boolean. Only applicable if `gfm` is enabled. Makes all line
      breaks significant (so every line in the input is a new line in the output)

    * `:smartypants` - boolean. Turns on smartypants processing, so quotes become curly,
      two or three hyphens become en and em dashes, and so on. False by default

    * `:highlight_tag` - the tag used for highlighting code, defaults to "span"

  """
  def to_html(text, opts) do
    options =
      struct(Earmark.Options,
        gfm: Keyword.get(opts, :gfm, true),
        line: Keyword.get(opts, :line, 1),
        file: Keyword.get(opts, :file),
        breaks: Keyword.get(opts, :breaks, false),
        smartypants: Keyword.get(opts, :smartypants, false),
        pure_links: true
      )

    text
    |> Earmark.as_html!(options)
    |> ExDoc.Highlighter.highlight_code_blocks(opts)
  end
end
