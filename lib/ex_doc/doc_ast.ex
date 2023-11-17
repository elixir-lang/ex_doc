defmodule ExDoc.DocAST do
  @moduledoc false

  @type t :: term()

  alias ExDoc.Markdown

  @doc """
  Parses given `doc_content` according to `doc_format`.
  """
  def parse!(doc_content, doc_format, options \\ [])

  def parse!(markdown, "text/markdown", opts) do
    parse_markdown(markdown, opts)
  end

  def parse!(ast, "application/erlang+html", _options) do
    parse_erl_ast(ast)
  end

  def parse!(_ast, other, _opts) do
    raise "content type #{inspect(other)} is not supported"
  end

  # https://www.w3.org/TR/2011/WD-html-markup-20110113/syntax.html#void-element
  @void_elements ~W(area base br col command embed hr img input keygen link
    meta param source track wbr)a

  @doc """
  Transform AST into string.
  """
  def to_string(ast, fun \\ fn _ast, string -> string end)

  def to_string(binary, _fun) when is_binary(binary) do
    ExDoc.Utils.h(binary)
  end

  def to_string(list, fun) when is_list(list) do
    result = Enum.map_join(list, "", &to_string(&1, fun))
    fun.(list, result)
  end

  def to_string({:comment, _attrs, inner, _meta} = ast, fun) do
    fun.(ast, "<!--#{inner}-->")
  end

  def to_string({tag, attrs, _inner, _meta} = ast, fun) when tag in @void_elements do
    result = "<#{tag}#{ast_attributes_to_string(attrs)}/>"
    fun.(ast, result)
  end

  def to_string({tag, attrs, inner, %{verbatim: true}} = ast, fun) do
    inner = Enum.join(inner, "")
    result = "<#{tag}#{ast_attributes_to_string(attrs)}>" <> inner <> "</#{tag}>"
    fun.(ast, result)
  end

  def to_string({tag, attrs, inner, _meta} = ast, fun) do
    result = "<#{tag}#{ast_attributes_to_string(attrs)}>" <> to_string(inner, fun) <> "</#{tag}>"
    fun.(ast, result)
  end

  defp ast_attributes_to_string(attrs) do
    Enum.map(attrs, fn {key, val} -> " #{key}=\"#{val}\"" end)
  end

  ## parse markdown

  defp parse_markdown(markdown, opts) do
    Markdown.to_ast(markdown, opts)
  end

  ## parse erlang+html

  defp parse_erl_ast(binary) when is_binary(binary) do
    binary
  end

  defp parse_erl_ast(list) when is_list(list) do
    Enum.map(list, &parse_erl_ast/1)
  end

  defp parse_erl_ast({:pre, attrs, content}) do
    case content do
      # if we already have <pre><code>...</code></pre>, carry on
      [{:code, _, _}] ->
        {:pre, attrs, parse_erl_ast(content), %{}}

      # otherwise, turn <pre>...</pre> into <pre><code>...</code></pre>
      _ ->
        content = [{:code, [], parse_erl_ast(content), %{}}]
        {:pre, attrs, content, %{}}
    end
  end

  defp parse_erl_ast({tag, attrs, content}) when is_atom(tag) do
    {tag, attrs, parse_erl_ast(content), %{}}
  end

  @doc """
  Extracts leading title element from the given AST.

  If found, the title element is stripped from the resulting AST.
  """
  def extract_title(ast)

  def extract_title([{:comment, _, _, _} | ast]), do: extract_title(ast)
  def extract_title([{:h1, _attrs, inner, _meta} | ast]), do: {:ok, inner, ast}
  def extract_title(_ast), do: :error

  @doc """
  Returns text content from the given AST.
  """
  def text_from_ast(ast) do
    ast
    |> do_text_from_ast()
    |> IO.iodata_to_binary()
    |> String.trim()
  end

  def do_text_from_ast(ast) when is_list(ast) do
    Enum.map(ast, &do_text_from_ast/1)
  end

  def do_text_from_ast(ast) when is_binary(ast), do: ast
  def do_text_from_ast({_tag, _attr, ast, _meta}), do: text_from_ast(ast)

  @doc """
  Wraps a list of HTML nodes into `<section>` tags whenever `matcher` returns true.
  """
  def sectionize(list, matcher), do: sectionize(list, matcher, [])

  defp sectionize(list, matcher, acc) do
    case pivot(list, acc, matcher) do
      {acc, {header_tag, header_attrs, _, _} = header, rest} ->
        {inner, rest} = Enum.split_while(rest, &not_tag?(&1, header_tag))
        class = String.trim_trailing("#{header_tag} #{header_attrs[:class]}")
        section = {:section, [class: class], [header | sectionize(inner, matcher, [])], %{}}
        sectionize(rest, matcher, [section | acc])

      acc ->
        acc
    end
  end

  defp not_tag?({tag, _, _, _}, tag), do: false
  defp not_tag?(_, _tag), do: true

  defp pivot([head | tail], acc, fun) do
    case fun.(head) do
      true -> {acc, head, tail}
      false -> pivot(tail, [head | acc], fun)
    end
  end

  defp pivot([], acc, _fun), do: Enum.reverse(acc)

  @doc """
  Highlights a DocAST converted to string.
  """
  def highlight(html, language, opts \\ []) do
    highlight_info = language.highlight_info()

    ## Html cannot be parsed with regex, but we try our best...
    Regex.replace(
      ~r/<pre(\s[^>]*)?><code(?:\s+class="(\w*)")?>([^<]*)<\/code><\/pre>/,
      html,
      &highlight_code_block(&1, &2, &3, &4, highlight_info, opts)
    )
  end

  defp highlight_code_block(full_block, pre_attr, lang, code, highlight_info, outer_opts) do
    case pick_language_and_lexer(lang, highlight_info, code) do
      {_language, nil, _opts} ->
        full_block

      {lang, lexer, opts} ->
        try do
          render_code(pre_attr, lang, lexer, opts, code, outer_opts)
        rescue
          _ ->
            ExDoc.Utils.warn(
              [
                "crashed while highlighting #{lang} snippet:\n\n",
                full_block
              ],
              __STACKTRACE__
            )

            full_block
        end
    end
  end

  defp pick_language_and_lexer("", _highlight_info, "$ " <> _) do
    {"shell", ExDoc.ShellLexer, []}
  end

  defp pick_language_and_lexer("output", highlight_info, _code) do
    {"output", highlight_info.lexer, highlight_info.opts}
  end

  defp pick_language_and_lexer("", highlight_info, _code) do
    {highlight_info.language_name, highlight_info.lexer, highlight_info.opts}
  end

  defp pick_language_and_lexer(lang, _highlight_info, _code) do
    case Makeup.Registry.fetch_lexer_by_name(lang) do
      {:ok, {lexer, opts}} -> {lang, lexer, opts}
      :error -> {lang, nil, []}
    end
  end

  defp render_code(pre_attr, lang, lexer, lexer_opts, code, opts) do
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

    ~s(<pre#{pre_attr}><code class="makeup #{lang}" translate="no">#{highlighted}</code></pre>)
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
