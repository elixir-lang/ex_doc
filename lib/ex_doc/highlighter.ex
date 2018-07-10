defmodule ExDoc.Highlighter do
  @moduledoc """
  Performs code highlighting.
  """

  @doc """
  Highlighter specific assets.
  """
  def assets(_), do: []

  @doc """
  Highlighter specific annotations.
  """
  def before_closing_head_tag(_), do: ""

  @doc """
  Highlighter specific annotations.
  """
  def before_closing_body_tag(_), do: ""

  # If new lexers are available, add them here:
  defp pick_language_and_lexer(""), do: {"elixir", Makeup.Lexers.ElixirLexer}
  defp pick_language_and_lexer("elixir"), do: {"elixir", Makeup.Lexers.ElixirLexer}
  defp pick_language_and_lexer(other), do: {other, nil}

  @doc """
  Highlights all code block in an already generated HTML document.
  """
  def highlight_code_blocks(html) do
    Regex.replace(
      ~r/<pre><code(?:\s+class="(\w*)")?>([^<]*)<\/code><\/pre>/,
      html,
      &highlight_code_block/3
    )
  end

  defp highlight_code_block(full_block, lang, code) do
    case pick_language_and_lexer(lang) do
      {_language, nil} -> full_block
      {language, lexer} -> render_code(language, lexer, code)
    end
  end

  defp render_code(lang, lexer, code) do
    highlighted =
      code
      |> unescape_html()
      |> IO.iodata_to_binary()
      |> Makeup.highlight_inner_html(lexer: lexer)

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
