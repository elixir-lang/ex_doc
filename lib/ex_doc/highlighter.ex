defmodule ExDoc.Highlighter do
  @moduledoc false

  alias Makeup.Formatters.HTML.HTMLFormatter

  @external_resource "assets/dist/ex_doc_makeup.css"

  @assets [
    # Read the CSS from the included file.
    # This allows us to have a custom CSS theme not included in Makeup
    # that supports both "day mode" and "night mode".
    {"dist/ex_doc_makeup-css.css", File.read!("assets/dist/ex_doc_makeup.css")},
    # Get the Javascript snippet directly from Makeup.
    # If there is any need to customize it further, we can add a "ex_doc_makeup.js" file.
    {"dist/ex_doc_makeup-js.js", HTMLFormatter.group_highlighter_javascript()}
  ]

  def assets(_), do: @assets

  def before_closing_head_tag(_), do: ~S(<link rel="stylesheet" href="dist/ex_doc_makeup-css.css"/>)

  def before_closing_body_tag(_), do: ~S(<script src="dist/ex_doc_makeup-js.js"></script>)

  # If new lexers are available, add them here:
  defp pick_language_and_lexer(""), do: {"elixir", Makeup.Lexers.ElixirLexer}
  defp pick_language_and_lexer("elixir"), do: {"elixir", Makeup.Lexers.ElixirLexer}
  defp pick_language_and_lexer(other), do: {other, nil}

  # Public API for the module.
  # Highlights all code block in an already generated HTML document.
  def highlight_code_blocks(html) do
    Regex.replace(~r/<pre><code(?:\s+class="(\w*)")?>([^<]*)<\/code><\/pre>/, html, &highlight_code_block/3)
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
