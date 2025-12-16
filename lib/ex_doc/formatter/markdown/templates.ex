defmodule ExDoc.Formatter.MARKDOWN.Templates do
  @moduledoc false

  require EEx

  import ExDoc.Utils,
    only: [before_closing_body_tag: 2, h: 1]

  @doc """
  Generate content from the module template for a given `node`.
  """
  def module_page(config, module_node) do
    docs =
      for group <- module_node.docs_groups do
        {group.title, group.docs}
      end

    module_template(config, module_node, docs)
  end

  @doc """
  Formats the attribute type used to define the spec of the given `node`.
  """
  def format_spec_attribute(module, node) do
    module.language.format_spec_attribute(node)
  end

  @doc """
  Returns the original markdown documentation from source_doc.
  """
  def node_doc(%{source_doc: %{"en" => source}}) when is_binary(source), do: source
  def node_doc(_), do: nil

  @doc """
  Creates a chapter which contains all the details about an individual module.
  """
  EEx.function_from_file(
    :def,
    :module_template,
    Path.expand("templates/module_template.eex", __DIR__),
    [:config, :module, :docs],
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

  @doc false
  EEx.function_from_file(
    :def,
    :detail_template,
    Path.expand("templates/detail_template.eex", __DIR__),
    [:node, :module],
    trim: true
  )
end
