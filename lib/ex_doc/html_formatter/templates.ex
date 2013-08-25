defmodule ExDoc.HTMLFormatter.Templates do
  @moduledoc """
  Handle all template interfaces for the HTMLFormatter.
  """

  require EEx
  alias ExDoc.HTMLFormatter.Autolink

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(node) do
    types     = node.typespecs
    functions = Enum.filter node.docs, &match?(ExDoc.FunctionNode[type: :def], &1)
    macros    = Enum.filter node.docs, &match?(ExDoc.FunctionNode[type: :defmacro], &1)
    callbacks = Enum.filter node.docs, &match?(ExDoc.FunctionNode[type: :defcallback], &1)
    module_template(node, types, functions, macros, callbacks)
  end

  @doc """
  Generates the listing.
  """
  def list_page(scope, nodes, config, has_readme) do
    list_template(scope, nodes, config, has_readme)
  end

  # Get fields for records an exceptions, removing any field
  # that starts with underscore
  defp get_fields(ExDoc.ModuleNode[type: type] = node) when type in [:record, :exception] do
    node.module.__record__(:fields)
    |> Enum.filter(fn({f,_}) -> hd(atom_to_list(f)) != ?_ end)
    |> presence
  end

  defp get_fields(_), do: nil

  # Get the full specs from a function, already in HTML form.
  defp get_specs(ExDoc.FunctionNode[specs: specs]) when is_list(specs) do
    presence specs
  end

  defp get_specs(_node), do: nil

  # Convert markdown to HTML.
  defp to_html(nil), do: nil
  defp to_html(bin) when is_binary(bin), do: Markdown.to_html(bin)

  # Get the full signature from a function
  defp signature(ExDoc.FunctionNode[name: name, signature: args]) do
    Macro.to_string { name, 0, args }
  end

  # Get the first paragraph of the documentation of a node, if any.
  defp synopsis(node) do
    if node.doc != nil do
      String.split(node.doc, %r/\n\s*\n/) |> hd
    else
      nil
    end
  end

  defp presence([]),    do: nil
  defp presence(other), do: other

  defp h(binary) do
    escape_map = [{ %r(&), "\\&amp;" }, { %r(<), "\\&lt;" }, { %r(>), "\\&gt;" }, { %r("), "\\&quot;" }]
    Enum.reduce escape_map, binary, fn({ re, escape }, acc) -> Regex.replace(re, acc, escape) end
  end

  templates = [
    index_template: [:config],
    list_template: [:scope, :nodes, :config, :has_readme],
    module_template: [:module, :types, :functions, :macros, :callbacks],
    list_item_template: [:node],
    summary_template: [:node],
    detail_template: [:node, :_module],
    type_detail_template: [:node, :_module],
    readme_template: [:content]
  ]

  Enum.each templates, fn({ name, args }) ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    EEx.function_from_file :def, name, filename, args
  end
end
