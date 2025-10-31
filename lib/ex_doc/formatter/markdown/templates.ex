defmodule ExDoc.Formatter.MARKDOWN.Templates do
  @moduledoc false

  require EEx

  import ExDoc.Utils,
    only: [before_closing_body_tag: 2, h: 1, text_to_id: 1]

  @doc """
  Generate content from the module template for a given `node`.
  """
  def module_page(config, module_node) do
    summary =
      for group <- module_node.docs_groups do
        {group.title, group.docs}
      end

    module_template(config, module_node, summary)
  end

  @doc """
  Returns the formatted title for the module page.
  """
  def module_type(%{type: :task} = _node), do: ""
  def module_type(%{type: :module} = _node), do: ""
  def module_type(%{type: type} = _node), do: "(#{type})"

  @doc """
  Format the attribute type used to define the spec of the given `node`.
  """
  def format_spec_attribute(module, node) do
    module.language.format_spec_attribute(node)
  end

  @doc """
  Generated ID for static file.
  """
  def static_file_to_id(static_file) do
    static_file |> Path.basename() |> text_to_id()
  end

  def node_doc(%{doc: doc}) when is_list(doc) do
    # Handle DocAST by converting to markdown
    ExDoc.DocAST.to_markdown(doc)
  end

  def node_doc(%{doc: doc}) when is_binary(doc), do: doc
  def node_doc(%{source_doc: %{"en" => source}}) when is_binary(source), do: source
  def node_doc(%{rendered_doc: source}) when is_binary(source), do: source

  def node_doc(%{source_doc: %{"en" => source}}) when is_list(source) do
    # Handle DocAST by converting to markdown
    # For Erlang docs, we can extract text content
    extract_text_from_doc_ast(source)
  end

  def node_doc(_), do: nil

  @doc """
  Get synopsis for a node, handling both DocAST and string documentation.
  Uses DocAST synopsis extraction logic for consistency with HTML formatter.
  """
  def node_synopsis(%{doc: doc}) when is_list(doc) do
    # For DocAST, extract synopsis DocAST and convert to markdown
    case extract_synopsis_ast(doc) do
      nil -> nil
      synopsis_ast -> ExDoc.DocAST.to_markdown(synopsis_ast)
    end
  end

  def node_synopsis(%{doc: doc}) when is_binary(doc) do
    synopsis(doc)
  end

  def node_synopsis(%{source_doc: %{"en" => source}}) when is_binary(source) do
    synopsis(source)
  end

  def node_synopsis(%{rendered_doc: source}) when is_binary(source) do
    synopsis(source)
  end

  def node_synopsis(%{source_doc: %{"en" => source}}) when is_list(source) do
    # For Erlang DocAST, extract synopsis and convert to plain text
    case extract_synopsis_ast(source) do
      nil -> nil
      synopsis_ast -> synopsis_ast |> ExDoc.DocAST.to_markdown() |> extract_plain_text()
    end
  end

  def node_synopsis(_), do: nil

  # Extract synopsis as DocAST (similar to ExDoc.DocAST.synopsis but returns an AST instead of an HTML string).
  defp extract_synopsis_ast({:p, _attrs, [_ | _] = inner, _meta}) do
    inner =
      case Enum.split(inner, -1) do
        {pre, [post]} when is_binary(post) ->
          pre ++ [String.trim_trailing(post, ":")]

        _ ->
          inner
      end

    {:p, [], ExDoc.DocAST.remove_ids(inner), %{}}
  end

  defp extract_synopsis_ast([head | _]), do: extract_synopsis_ast(head)
  defp extract_synopsis_ast(_other), do: nil

  defp extract_text_from_doc_ast(ast) when is_list(ast) do
    Enum.map_join(ast, "\n\n", &extract_text_from_doc_ast/1)
  end

  defp extract_text_from_doc_ast({_tag, _attrs, content}) when is_list(content) do
    Enum.map_join(content, "", &extract_text_from_doc_ast/1)
  end

  defp extract_text_from_doc_ast({_tag, _attrs, content, _meta}) when is_list(content) do
    Enum.map_join(content, "", &extract_text_from_doc_ast/1)
  end

  defp extract_text_from_doc_ast(text) when is_binary(text), do: text
  defp extract_text_from_doc_ast(_), do: ""

  # Extract plain text from markdown (similar to the one in markdown.ex but here for templates)
  defp extract_plain_text(markdown) when is_binary(markdown) do
    markdown
    |> String.replace(~r/<[^>]*>/, "")
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
    |> case do
      "" ->
        nil

      text ->
        text
        |> String.slice(0, 150)
        |> then(fn s -> if String.length(s) == 150, do: s <> "...", else: s end)
    end
  end

  defp extract_plain_text(_), do: nil

  @doc """
  Gets the first paragraph of the documentation of a node. It strips
  surrounding white-spaces and trailing `:`.

  If `doc` is `nil`, it returns `nil`.
  """
  @spec synopsis(String.t()) :: String.t()
  @spec synopsis(nil) :: nil
  def synopsis(doc) when is_binary(doc) do
    case :binary.split(doc, "\n\n") do
      [left, _] -> String.trim_trailing(left, ": ") <> "\n\n"
      [all] -> all
    end
  end

  def synopsis(_), do: nil

  @heading_regex ~r/^(\#{1,6})\s+(.*)/m
  defp rewrite_headings(content) when is_binary(content) do
    @heading_regex
    |> Regex.scan(content)
    |> Enum.reduce(content, fn [match, level, title], content ->
      replacement = rewrite_heading(level, title)
      String.replace(content, match, replacement, global: false)
    end)
  end

  defp rewrite_headings(_), do: nil

  defp rewrite_heading("#", title), do: do_rewrite_heading("#####", title)
  defp rewrite_heading(_, title), do: do_rewrite_heading("######", title)

  defp do_rewrite_heading(level, title) do
    """
    #{level} #{title}
    """
  end

  defp enc(binary), do: URI.encode(binary) |> String.replace("/", "-")

  @doc """
  Creates a chapter which contains all the details about an individual module.

  This chapter can include the following sections: *functions*, *types*, *callbacks*.
  """
  EEx.function_from_file(
    :def,
    :module_template,
    Path.expand("templates/module_template.eex", __DIR__),
    [:config, :module, :summary],
    trim: true
  )

  @doc """
  Creates the table of contents.
  """
  EEx.function_from_file(
    :def,
    :nav_template,
    Path.expand("templates/nav_template.eex", __DIR__),
    [:config, :nodes],
    trim: true
  )

  EEx.function_from_file(
    :defp,
    :nav_item_template,
    Path.expand("templates/nav_item_template.eex", __DIR__),
    [:name, :nodes],
    trim: true
  )

  EEx.function_from_file(
    :defp,
    :nav_grouped_item_template,
    Path.expand("templates/nav_grouped_item_template.eex", __DIR__),
    [:nodes],
    trim: true
  )

  # EEx.function_from_file(
  #   :defp,
  #   :toc_item_template,
  #   Path.expand("templates/toc_item_template.eex", __DIR__),
  #   [:nodes],
  #   trim: true
  # )

  # def media_type(_arg), do: nil

  templates = [
    detail_template: [:node, :module],
    summary_template: [:name, :nodes]
  ]

  Enum.each(templates, fn {name, args} ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    @doc false
    EEx.function_from_file(:def, name, filename, args, trim: true)
  end)
end
