defmodule ExDoc.Markdown.Earmark do
  @moduledoc """
  ExDoc extension for the EarmarkParser Markdown parser.
  """
  @behaviour ExDoc.Markdown

  @doc """
  Check if the EarmarkParser Markdown parser module is available.
  """
  def available? do
    match?({:ok, _}, Application.ensure_all_started(:earmark_parser)) and
      Code.ensure_loaded?(EarmarkParser)
  end

  @doc """
  Generate HTML AST.

  ## Options

    * `:gfm` - boolean. Turns on Github Flavored Markdown extensions. True by default

    * `:breaks` - boolean. Only applicable if `gfm` is enabled. Makes all line
      breaks significant (so every line in the input is a new line in the output)

    * `:smartypants` - boolean. Turns on smartypants processing, so quotes become curly,
      two or three hyphens become en and em dashes, and so on. False by default

  """
  @impl true
  def to_ast(text, opts) when is_binary(text) do
    options = [
      gfm: Keyword.get(opts, :gfm, true),
      line: Keyword.get(opts, :line, 1),
      file: Keyword.get(opts, :file, "nofile"),
      breaks: Keyword.get(opts, :breaks, false),
      smartypants: Keyword.get(opts, :smartypants, false),
      pure_links: true
    ]

    as_ast(text, options)
  end

  defp as_ast(binary, options, meta \\ %{}) do
    {response, ast, messages} = EarmarkParser.as_ast(binary, options)
    print_messages(messages, options)

    verbatim = meta[:verbatim] || false

    case response do
      :ok when verbatim and is_tuple(ast) ->
        case ast do
          {tag, attrs, children, ast_meta} ->
            fixup(
              {tag, attrs, children, Kernel.put_in(ast_meta, :verbatim, true)},
              options,
              meta
            )

          {tag, attrs, children} ->
            fixup({tag, attrs, children, %{meta: %{verbatim: true}}}, options, meta)
        end

      :ok ->
        fixup(ast, options, meta)

      :error ->
        ast
    end
  end

  defp print_messages(messages, options) do
    for {severity, line, message} <- messages do
      file = options.file
      IO.warn("#{inspect(__MODULE__)} (#{severity}) #{file}:#{line} #{message}", [])
    end
  end

  defp fixup(list, options, meta) when is_list(list) do
    fixup_list(list, [], options, meta)
  end

  defp fixup(binary, _options, _meta) when is_binary(binary) do
    binary
  end

  defp fixup({tag, attrs, ast, ast_meta}, options, meta) when is_binary(tag) do
    verbatim = meta[:verbatim] || ast_meta[:meta][:verbatim] || false

    {
      fixup_tag(tag),
      fixup_meta(ast_meta),
      Enum.map(attrs, &fixup_attr/1),
      fixup(
        ast,
        options,
        Map.put(meta, :verbatim, verbatim)
      )
    }
  end

  defp fixup({tag, attrs, ast}, options, meta) when is_binary(tag) do
    fixup({tag, attrs, ast, %{}}, options, meta)
  end

  defp fixup({:comment, _, children, %{comment: true}}, _options, meta) do
    {nil, Map.put(meta, :comment, true), [], children}
  end

  defp fixup_list([head | tail], acc, options, meta) do
    case fixup(head, options, meta) do
      [] ->
        fixup_list(tail, acc, options, meta)

      fixed ->
        fixup_list(tail, [fixed | acc], options, meta)
    end
  end

  defp fixup_list([], acc, _options, _meta) do
    Enum.reverse(acc)
  end

  defp fixup_tag(tag) do
    String.to_atom(tag)
  end

  defp fixup_attr({name, value}) do
    {String.to_atom(name), to_string(value)}
  end

  defp fixup_meta(meta) do
    fixup_meta(meta, %{})
  end

  defp fixup_meta(%{line_number: line_number} = meta, acc),
    do: fixup_meta(Map.delete(meta, :line_number), Map.put(acc, :line_number, line_number))

  defp fixup_meta(%{lnb: line_number} = meta, acc),
    do: fixup_meta(Map.delete(meta, :lnb), Map.put(acc, :line_number, line_number))

  defp fixup_meta(%{meta: %{verbatim: verbatim}} = meta, acc),
    # Delete the whole meta, since it only holds :verbatim
    do: fixup_meta(Map.delete(meta, :meta), Map.put(acc, :verbatim, verbatim))

  defp fixup_meta(%{verbatim: verbatim} = meta, acc),
    do: fixup_meta(Map.delete(meta, :verbatim), Map.put(acc, :verbatim, verbatim))

  defp fixup_meta(%{comment: comment} = meta, acc),
    do: fixup_meta(Map.delete(meta, :comment), Map.put(acc, :comment, comment))

  defp fixup_meta(meta, acc),
    do: Map.merge(meta, acc)
end
