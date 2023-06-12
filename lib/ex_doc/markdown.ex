defmodule ExDoc.Markdown do
  @moduledoc """
  Adapter behaviour and conveniences for converting Markdown to HTML.

  ExDoc is compatible with any markdown processor that implements the
  functions defined in this module. The markdown processor can be changed
  via the `:markdown_processor` option in your `mix.exs`.

  ExDoc supports the following Markdown parsers out of the box:

    * [EarmarkParser](https://github.com/robertdober/earmark_parser)

  ExDoc uses EarmarkParser by default.
  """

  @doc """
  Converts markdown into HTML.
  """
  @callback to_ast(String.t(), Keyword.t()) :: term()

  @doc """
  Returns true if all dependencies necessary are available.
  """
  @callback available?() :: boolean()

  @markdown_processors [
    ExDoc.Markdown.Earmark
  ]

  @markdown_processor_key :markdown_processor

  @doc """
  Converts the given markdown document to HTML AST.
  """
  def to_ast(text, opts \\ []) when is_binary(text) do
    {processor, options} = get_markdown_processor()

    to_ast(text, processor, Keyword.merge(options, opts), [])
  end

  defp to_ast(text, processor, opts, applied_processors) do
    text
    |> processor.to_ast(opts)
    |> apply_fence_processors(processor, opts, applied_processors)
  end

  @doc """
  Gets the current markdown processor set globally.
  """
  def get_markdown_processor do
    case Application.fetch_env(:ex_doc, @markdown_processor_key) do
      {:ok, {processor, options}} ->
        {processor, options}

      :error ->
        processor = find_markdown_processor() || raise_no_markdown_processor()
        put_markdown_processor({processor, []})
        {processor, []}
    end
  end

  @doc """
  Changes the markdown processor globally.
  """
  def put_markdown_processor(processor) when is_atom(processor) do
    put_markdown_processor({processor, []})
  end

  def put_markdown_processor({processor, options}) do
    Application.put_env(:ex_doc, @markdown_processor_key, {processor, options})
  end

  defp find_markdown_processor do
    Enum.find(@markdown_processors, fn module ->
      Code.ensure_loaded?(module) && module.available?
    end)
  end

  defp raise_no_markdown_processor do
    raise """
    Could not find a markdown processor to be used by ex_doc.
    You can either:

    * Add {:earmark, ">= 0.0.0"} to your mix.exs deps
      to use an Elixir-based markdown processor
    """
  end

  @doc """
  Wraps the desired tags in HTML in sections.
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

  defp apply_fence_processors(ast, processor, opts, applied_processors) do
    Enum.reduce(ast, [], fn block, acc ->
      acc ++ maybe_apply_fence_processors(block, processor, opts, applied_processors)
    end)
  end

  defp maybe_apply_fence_processors(
         {:pre, pre_attrs, [{:code, [class: fence], content, _code_meta}], _pre_meta} = block,
         processor,
         opts,
         applied_processors
       ) do
    fence_processors = Keyword.get(opts, :fence_processors, %{})

    cond do
      # we want to avoid infinite recursion here, just return the block
      fence in applied_processors ->
        [block]

      # if we have defined a custom fence processor apply it (recursively)
      Map.has_key?(fence_processors, fence) ->
        # for now it expects to be a mfa tuple - to be discussed
        {module, function, args} = fence_processors[fence]

        code = Enum.join(content, "\n")

        apply(module, function, [code | args])
        |> to_ast(processor, opts, [fence | applied_processors])

      # in any other case return the original block
      true ->
        [block]
    end
  end

  defp maybe_apply_fence_processors(block, _processor, _opts, _applied_processors), do: [block]
end
