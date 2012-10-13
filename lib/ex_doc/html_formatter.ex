defmodule ExDoc.HTMLFormatter do
  require EEx

  def assets do
    [
      { templates_path("index.html"), "." },
      { templates_path("css/*.css"), "css" },
      { templates_path("js/*.js"), "js" }
    ]
  end

  def module_page(node) do
    functions = Enum.filter node.docs, match?(ExDoc.FunctionNode[type: :def], &1)
    macros    = Enum.filter node.docs, match?(ExDoc.FunctionNode[type: :defmacro], &1)
    callbacks = Enum.filter node.docs, match?(ExDoc.FunctionNode[type: :defcallback], &1)
    fields    = get_fields(node)
    impls     = get_impls(node)
    module_template(node, functions, macros, callbacks, fields, impls)
  end

  def list_page(scope, nodes) do
    list_template(scope, nodes)
  end

  # Get only fields that start with underscore
  defp get_fields(ExDoc.ModuleNode[type: type] = node) when type in [:record, :exception] do
    Enum.filter node.module.__record__(:fields), fn({f,_}) ->
      hd(atom_to_list(f)) != ?_
    end
  end

  defp get_fields(_), do: []

  # Loop through all children finding implementations
  defp get_impls(ExDoc.ModuleNode[type: :protocol, module: module, children: children]) do
    Enum.filter children, fn(child) ->
      child.type == :impl && child.module.__impl__ == module
    end
  end

  defp get_impls(_), do: []

  # Convert to html using markdown
  defp to_html(nil), do: nil
  defp to_html(bin) when is_binary(bin), do: Markdown.to_html(bin)

  # Get the full signature from a function
  defp signature(ExDoc.FunctionNode[name: name, signature: args]) do
    name = atom_to_binary(name)
    args = Enum.map_join(args, ", ", signature_arg(&1))
    "#{name}(#{args})"
  end

  defp signature(node) do
    node.id
  end

  defp signature_arg({ ://, _, [left, right] }) do
    signature_arg(left) <> " // " <> Macro.to_binary(right)
  end

  defp signature_arg({ var, _, _ }) do
    atom_to_binary(var)
  end

  # Escaping
  defp h(binary) do
    escape_map = [{ %r(&), "\\&amp;" }, { %r(<), "\\&lt;" }, { %r(>), "\\&gt;" }, { %r("), "\\&quot;" }]
    Enum.reduce escape_map, binary, fn({ re, escape }, acc) -> Regex.replace(re, acc, escape) end
  end

  templates = [
    module_template: [:module, :functions, :macros, :callbacks, :fields, :impls],
    list_template: [:scope, :nodes],
    list_item_template: [:node],
    summary_template: [:node],
    detail_template: [:node]
  ]

  defp templates_path(other) do
    File.expand_path("../../templates/#{other}", __FILE__)
  end

  Enum.each templates, fn({ name, args }) ->
    filename = File.expand_path("../../templates/#{name}.eex", __FILE__)
    EEx.function_from_file :defp, name, filename, args
  end
end
