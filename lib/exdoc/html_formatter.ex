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
    fields    = get_fields(node)
    module_template(node, functions, macros, fields)
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

  defp to_html(nil), do: nil
  defp to_html(bin) when is_binary(bin), do: Markdown.to_html(bin)

  defp templates_path(other) do
    File.expand_path("../../templates/#{other}", __FILE__)
  end

  templates = [
    module_template: [:module, :functions, :macros, :fields],
    list_template: [:scope, :nodes],
    list_item_template: [:node],
    summary_template: [:node],
    detail_template: [:node]
  ]

  Enum.each templates, fn({ name, args }) ->
    filename = File.expand_path("../../templates/#{name}.eex", __FILE__)
    EEx.function_from_file :defp, name, filename, args
  end
end