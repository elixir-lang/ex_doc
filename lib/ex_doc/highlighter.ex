defmodule ExDoc.Highlighter do
  @moduledoc false

  # If new lexers are available, add them here:
  defp pick_lexer("elixir"), do: Makeup.Lexers.ElixirLexer
  defp pick_lexer(_other), do: nil

  # Public API for the module.
  # Highlights all code block in an already generated HTML document.
  def highlight_code_blocks(html) do
    Regex.replace(~r/<pre><code class="(\w+)">([^<]*)<\/code><\/pre>/, html, &highlight_code_block/3)
  end

  defp highlight_code_block(full_block, lang, code) do
    case pick_lexer(lang) do
      nil -> full_block
      lexer -> render_code(lang, lexer, code)
    end
  end

  defp render_code(lang, lexer, code) do
    highlighted =
      code
      |> unescape_html_entities()
      |> Makeup.highlight_inner_html(lexer: lexer)
    ~s(<pre><code class="nohighlight makeup #{lang}">#{highlighted}</code></pre>)
  end

  # TODO: this implementation is probably not very efficient.
  # benchmark and make this faster if apropriate
  defp unescape_html_entities(string) do
    escape_map = [{"&amp;", "&"}, {"&lt;", ">"}, {"&gt;", ">"}, {"&quot;", ~S(")}, {"&#39;", "'"}]
    Enum.reduce escape_map, string, fn {pattern, escape}, acc ->
      String.replace(acc, pattern, escape)
    end
  end
end
