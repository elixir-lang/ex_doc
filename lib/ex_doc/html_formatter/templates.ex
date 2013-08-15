defmodule ExDoc.HTMLFormatter.Templates do
  @moduledoc """
  Handle all template interfaces for the HTMLFormatter.
  """

  require EEx

  # Convert to html using markdown
  defp to_html(nil, _node), do: nil
  defp to_html(bin, node) when is_binary(bin) do
    available_funs = node.docs 
      |> Enum.filter(&match?(ExDoc.FunctionNode[], &1))
      |> Enum.reduce([], fn(x, acc) -> [x.id|acc] end)

    bin |> Markdown.autolink_locals(available_funs) |> Markdown.to_html
  end

  # Get the full signature from a function
  defp signature(ExDoc.FunctionNode[name: name, signature: args]) do
    Macro.to_string { name, 0, args }
  end

  defp signature(node) do
    node.id
  end

  # Escaping
  defp h(binary) do
    escape_map = [{ %r(&), "\\&amp;" }, { %r(<), "\\&lt;" }, { %r(>), "\\&gt;" }, { %r("), "\\&quot;" }]
    Enum.reduce escape_map, binary, fn({ re, escape }, acc) -> Regex.replace(re, acc, escape) end
  end

  templates = [
    index_template: [:config],
    list_template: [:scope, :nodes, :config, :has_readme],
    module_template: [:module, :functions, :macros, :callbacks, :fields, :impls],
    list_item_template: [:node],
    summary_template: [:node],
    detail_template: [:node, :module],
    readme_template: [:content]
  ]

  def templates_path(other) do
    Path.expand("../../../templates/#{other}", __FILE__)
  end

  Enum.each templates, fn({ name, args }) ->
    filename = Path.expand("../../../templates/#{name}.eex", __FILE__)
    EEx.function_from_file :def, name, filename, args
  end
end
