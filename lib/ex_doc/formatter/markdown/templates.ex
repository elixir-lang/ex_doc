defmodule ExDoc.Formatter.MARKDOWN.Templates do
  @moduledoc false

  require EEx

  import ExDoc.Utils,
    only: [before_closing_body_tag: 2, h: 1, text_to_id: 1]

  alias ExDoc.Formatter.HTML.Templates, as: H

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(config, module_node) do
    summary = H.module_summary(module_node)
    module_template(config, module_node, summary)
  end

  @doc """
  Returns the formatted title for the module page.
  """
  def module_type(%{type: :task}), do: ""
  def module_type(%{type: :module}), do: ""
  def module_type(%{type: type}), do: "(#{type})"

  @doc """
  Generated ID for static file
  """
  def static_file_to_id(static_file) do
    static_file |> Path.basename() |> text_to_id()
  end

  def node_doc(%{source_doc: %{"en" => source}}), do: source
  def node_doc(%{rendered_doc: source}), do: source

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
