defmodule ExDoc.Highlighter do
  @moduledoc """
  Performs code highlighting.
  """

  # If new lexers are available, add them here:
  defp pick_language_and_lexer(""), do: {"elixir", Makeup.Lexers.ElixirLexer, []}

  defp pick_language_and_lexer(lang) do
    case Makeup.Registry.fetch_lexer_by_name(lang) do
      {:ok, {lexer, opts}} ->
        {lang, lexer, opts}

      :error ->
        {lang, nil, []}
    end
  end

  @doc """
  Highlights all code block in an already generated HTML document.
  """
  def highlight_code_blocks(html, opts \\ []) do
    Regex.replace(
      ~r/<pre><code(?:\s+class="(\w*)")?>([^<]*)<\/code><\/pre>/,
      html,
      &highlight_code_block(&1, &2, &3, opts)
    )
  end

  defp highlight_code_block(full_block, lang, code, outer_opts) do
    case pick_language_and_lexer(lang) do
      {_language, nil, _opts} -> full_block
      {language, lexer, opts} -> render_code(language, lexer, opts, code, outer_opts)
    end
  end

  defp render_code(lang, lexer, lexer_opts, code, opts) do
    highlight_tag = Keyword.get(opts, :highlight_tag, "span")

    highlighted =
      code
      |> unescape_html()
      |> IO.iodata_to_binary()
      |> Makeup.highlight_inner_html(
        lexer: lexer,
        lexer_options: lexer_opts,
        formatter_options: [highlight_tag: highlight_tag]
      )

    ~s(<pre><code class="nohighlight makeup #{lang}">#{highlighted}</code></pre>)
  end

  entities = [{"&amp;", ?&}, {"&lt;", ?<}, {"&gt;", ?>}, {"&quot;", ?"}, {"&#39;", ?'}]

  for {encoded, decoded} <- entities do
    defp unescape_html(unquote(encoded) <> rest) do
      [unquote(decoded) | unescape_html(rest)]
    end
  end

  defp unescape_html(<<c, rest::binary>>) do
    [c | unescape_html(rest)]
  end

  defp unescape_html(<<>>) do
    []
  end
end
