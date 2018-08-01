defmodule ExDoc.Markdown.Earmark do
  @moduledoc """
  ExDoc extension for the Earmark MarkDown parser.
  """
  @behaviour ExDoc.Markdown

  # Callback implementations
  def assets(arg), do: ExDoc.Highlighter.assets(arg)

  def before_closing_head_tag(arg), do: ExDoc.Highlighter.before_closing_head_tag(arg)

  def before_closing_body_tag(arg), do: ExDoc.Highlighter.before_closing_body_tag(arg)

  def configure(_), do: :ok

  @doc """
  Check if the Earmark Markdown parser module is available.
  """
  def available? do
    match?({:ok, _}, Application.ensure_all_started(:earmark)) and Code.ensure_loaded?(Earmark)
  end

  @doc """
  Earmark specific options:

    * `:gfm` - boolean. Turns on Github Flavored Markdown extensions. True by default

    * `:breaks` - boolean. Only applicable if `gfm` is enabled. Makes all line
      breaks significant (so every line in the input is a new line in the output)

    * `:smartypants` - boolean. Turns on smartypants processing, so quotes become curly,
      two or three hyphens become en and em dashes, and so on. True by default

    * `:plugins` - map of strings to modules. Register custom plugins to be used with
      Earmark. See [Plugins](http://github.com/pragdave/earmark#plugins) for details on
      how to write custom plugins.

  """
  def to_html(text, opts) do
    options =
      struct(Earmark.Options,
        gfm: Keyword.get(opts, :gfm, true),
        line: Keyword.get(opts, :line, 1),
        file: Keyword.get(opts, :file),
        breaks: Keyword.get(opts, :breaks, false),
        smartypants: Keyword.get(opts, :smartypants, true),
        plugins: Keyword.get(opts, :plugins, %{})
      )

    text
    |> Earmark.as_html!(options)
    |> ExDoc.Highlighter.highlight_code_blocks()
  end
end
