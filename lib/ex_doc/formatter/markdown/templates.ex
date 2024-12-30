defmodule ExDoc.Formatter.Markdown.Templates do
  @moduledoc false
  require EEx

  import ExDoc.Utils,
    only: [
      h: 1,
      text_to_id: 1
    ]

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(module_node, config) do
    summary = module_summary(module_node)
    module_template(config, module_node, summary)
  end

  @doc """
  Format the attribute type used to define the spec of the given `node`.
  """
  def format_spec_attribute(module, node) do
    module.language.format_spec_attribute(node)
  end

  @doc """
  Get the pretty name of a function node
  """
  def pretty_type(%{type: t}) do
    Atom.to_string(t)
  end

  @doc """
  Returns the HTML formatted title for the module page.
  """
  def module_type(%{type: :task}), do: ""
  def module_type(%{type: :module}), do: ""
  def module_type(%{type: type}), do: to_string(type)

  @doc """
  Gets the first paragraph of the documentation of a node. It strips
  surrounding white-spaces and trailing `:`.

  If `doc` is `nil`, it returns `nil`.
  """
  @spec synopsis(String.t()) :: String.t()
  @spec synopsis(nil) :: nil
  def synopsis(nil), do: nil

  def synopsis(doc) when is_binary(doc) do
    case :binary.split(doc, "\n\n") do
      [left, _] -> String.trim_trailing(left, ":") <> "\n\n"
      [all] -> all
    end
  end

  defp enc(binary), do: URI.encode(binary)

  def module_summary(module_node) do
    # TODO: Maybe it should be moved to retriever and it already returned grouped metadata
    ExDoc.GroupMatcher.group_by(module_node.docs_groups, module_node.docs, & &1.group)
  end

  def asset_rev(output, pattern) do
    output = Path.expand(output)

    output
    |> Path.join(pattern)
    |> Path.wildcard()
    |> relative_asset(output, pattern)
  end

  defp relative_asset([], output, pattern),
    do: raise("could not find matching #{output}/#{pattern}")

  defp relative_asset([h | _], output, _pattern), do: Path.relative_to(h, output)

  templates = [
    detail_template: [:config, :node, :module],
    footer_template: [:config],
    head_template: [:config, :page],
    module_template: [:config, :module, :summary],
    api_reference_entry_template: [:module_node],
    api_reference_template: [:nodes_map, :title],
    extra_template: [:config, :node, :type, :refs]
  ]

  Enum.each(templates, fn {name, args} ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    @doc false
    EEx.function_from_file(:def, name, filename, args, trim: true)
  end)
end
