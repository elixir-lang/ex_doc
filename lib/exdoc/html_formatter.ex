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
    module_template(node, functions, macros)
  end

  defp to_html(nil), do: nil
  defp to_html(bin) when is_binary(bin), do: Markdown.to_html(bin)

  defp templates_path(other) do
    File.expand_path("../../templates/#{other}", __FILE__)
  end

  filename = File.expand_path("../../templates/module_template.eex", __FILE__)
  EEx.function_from_file :defp, :module_template, filename, [:module, :functions, :macros]
end