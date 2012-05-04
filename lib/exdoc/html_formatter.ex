defmodule ExDoc.HTMLFormatter do
  require EEx

  def module_page(node) do
    functions = Enum.filter node.docs, match?(ExDoc.FunctionNode[type: :def], &1)
    macros    = Enum.filter node.docs, match?(ExDoc.FunctionNode[type: :defmacro], &1)
    module_template(node, functions, macros)
  end

  defp to_html(nil), do: nil
  defp to_html(bin) when is_binary(bin), do: Markdown.to_html(bin)

  filename = File.expand_path("../../templates/module_template.eex", __FILE__)
  EEx.function_from_file :defp, :module_template, filename, [:module, :functions, :macros]
end
