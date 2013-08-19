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

    bin |> ExDoc.Autolink.locals(available_funs) |> Markdown.to_html
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

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(node) do
    functions = Enum.filter node.docs, &match?(ExDoc.FunctionNode[type: :def], &1)
    macros    = Enum.filter node.docs, &match?(ExDoc.FunctionNode[type: :defmacro], &1)
    callbacks = Enum.filter node.docs, &match?(ExDoc.FunctionNode[type: :defcallback], &1)
    fields    = get_fields(node)
    impls     = get_impls(node)

    module_template(node, functions, macros, callbacks, fields, impls)
  end

  # Get only fields that do not start with underscore
  defp get_fields(ExDoc.ModuleNode[type: type] = node) when type in [:record, :exception] do
    Enum.filter node.module.__record__(:fields), fn({f,_}) ->
      hd(atom_to_list(f)) != ?_
    end
  end

  defp get_fields(_), do: []

  # Loop through all children finding implementations
  defp get_impls(ExDoc.ModuleNode[type: :protocol, module: module, children: children]) do
    Enum.filter children, fn(child) ->
      child.type == :impl && child.module.__impl__(:protocol) == module
    end
  end

  defp get_impls(_), do: []

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
