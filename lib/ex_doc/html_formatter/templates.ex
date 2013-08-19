defmodule ExDoc.HTMLFormatter.Templates do
  @moduledoc """
  Handle all template interfaces for the HTMLFormatter.
  """

  require EEx

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(node, _all) do
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

  # Get the full typespecs from a function, already in HTML form.
  #
  # This is distinct from typespec because a function can have multiple specs
  # while a type can only have one spec (a type is little more than a
  # spec).
  defp get_specs(ExDoc.FunctionNode[specs: specs]) when is_list(specs) do
    presence Enum.map(specs,
      &ExDoc.HTMLFormatter.LinkifyTypes.linkify(&1.spec, &1.locals, &1.remotes))
  end

  defp get_specs(_node), do: nil

  # Convert markdown to html with autolink.
  defp to_html(nil, _node), do: nil
  defp to_html(bin, node) when is_binary(bin) do
    available_funs = node.docs
      |> Enum.filter(&match?(ExDoc.FunctionNode[], &1))
      |> Enum.reduce([], fn(x, acc) -> [x.id|acc] end)

    bin |> ExDoc.Autolink.locals(available_funs) |> Markdown.to_html
  end

  # Get the label for a node (used in the summary).
  defp label(ExDoc.TypeNode[name: name]) do
    name
  end

  defp label(node) do
    node.id
  end

  # Get the full signature from a function
  defp signature(ExDoc.FunctionNode[name: name, signature: args]) do
    Macro.to_string { name, 0, args }
  end

  defp signature(node) do
    node.id
  end

  # Get the full typespec from a type, already in HTML form.
  # Returns HTML or nil if the node doesn't represent a type.
  defp typespec(ExDoc.TypeNode[spec: spec]) do
    ExDoc.HTMLFormatter.LinkifyTypes.linkify(spec.spec, spec.locals, spec.remotes)
  end

  defp typespec(_node) do
    nil
  end

  defp presence([]),    do: nil
  defp presence(other), do: other

  defp h(binary) do
    ExDoc.Autolink.escape_html(binary)
  end

  templates = [
    index_template: [:config],
    list_template: [:scope, :nodes, :config, :has_readme],
    module_template: [:module, :types, :functions, :macros, :callbacks],
    list_item_template: [:node],
    summary_template: [:node],
    detail_template: [:node, :module],
    type_detail_template: [:node],
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
