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
  Generate HTML AST.

  ## Options

    * `:gfm` - boolean. Turns on Github Flavored Markdown extensions. True by default

    * `:breaks` - boolean. Only applicable if `gfm` is enabled. Makes all line
      breaks significant (so every line in the input is a new line in the output)

    * `:smartypants` - boolean. Turns on smartypants processing, so quotes become curly,
      two or three hyphens become en and em dashes, and so on. False by default

  """
  @impl true
  def to_ast(text, opts) do
    options =
      struct(Earmark.Options,
        gfm: Keyword.get(opts, :gfm, true),
        line: Keyword.get(opts, :line, 1),
        file: Keyword.get(opts, :file, "nofile"),
        breaks: Keyword.get(opts, :breaks, false),
        smartypants: Keyword.get(opts, :smartypants, false),
        pure_links: true
      )

    case Earmark.as_ast(text, options) do
      {:ok, ast, messages} ->
        print_messages(messages, options)
        fixup(ast)

      {:error, ast, messages} ->
        print_messages(messages, options)
        ast
    end
  end

  defp print_messages(messages, options) do
    for {severity, line, message} <- messages do
      file = options.file
      IO.warn("#{inspect(__MODULE__)} (#{severity}) #{file}:#{line} #{message}", [])
    end
  end

  defp fixup(list) when is_list(list), do: Enum.map(list, &fixup/1)
  defp fixup(binary) when is_binary(binary), do: binary
  defp fixup({tag, attrs, ast}), do: {fixup_tag(tag), Enum.map(attrs, &fixup_attr/1), fixup(ast)}

  defp fixup_tag(tag), do: String.to_atom(tag)

  defp fixup_attr({name, value}), do: {String.to_atom(name), value}
end
