defmodule ExDoc.HTMLFormatter do
  def module_page(node) do
    functions = Enum.filter node.docs, match?(ExDoc.FunctionNode[type: :def], &1)
    macros    = Enum.filter node.docs, match?(ExDoc.FunctionNode[type: :defmacro], &1)
    bindings  = [module: node, functions: functions, macros: macros]
    EEx.eval_file("#{template_path}/module_template.eex", bindings)
  end

  defp template_path() do
    File.expand_path("../../templates", __FILE__)
  end
end
