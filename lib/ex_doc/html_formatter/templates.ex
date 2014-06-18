defmodule ExDoc.Formatter.HTML.Templates do
  @moduledoc """
  Handle all template interfaces for the HTML formatter.
  """

  require EEx

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(node, config, all) do
    types       = node.typespecs
    functions   = Enum.filter node.docs, &match?(%ExDoc.FunctionNode{type: :def}, &1)
    macros      = Enum.filter node.docs, &match?(%ExDoc.FunctionNode{type: :defmacro}, &1)
    callbacks   = Enum.filter node.docs, &match?(%ExDoc.FunctionNode{type: :defcallback}, &1)
    module_template(config, node, types, functions, macros, callbacks, all)
  end

  @doc """
  Generates the listing.
  """
  def list_page(scope, nodes, config, has_readme) do
    list_template(scope, nodes, config, has_readme)
  end

  # Get the full specs from a function, already in HTML form.
  defp get_specs(%ExDoc.FunctionNode{specs: specs}) when is_list(specs) do
    presence specs
  end

  defp get_specs(_node), do: nil

  # Convert markdown to HTML.
  defp to_html(nil), do: nil
  defp to_html(bin) when is_binary(bin), do: ExDoc.Markdown.to_html(bin)

  # Get the pretty name of a function node
  defp pretty_type(%ExDoc.FunctionNode{type: t}) do
    case t do
      :def          -> "function"
      :defmacro     -> "macro"
      :defcallback  -> "callback"
    end
  end

  # Get the first paragraph of the documentation of a node, if any.
  defp synopsis(nil), do: nil
  defp synopsis(doc) do
    String.split(doc, ~r/\n\s*\n/) |> hd |> String.strip() |> String.rstrip(?.)
  end

  # A bit of standard HTML to insert the to-top arrow.
  defp to_top_link() do
    "<a class=\"to_top_link\" href=\"#content\" title=\"To the top of the page\">&uarr;</a>"
  end

  defp presence([]),    do: nil
  defp presence(other), do: other

  defp h(binary) do
    escape_map = [{ ~r(&), "\\&amp;" }, { ~r(<), "\\&lt;" }, { ~r(>), "\\&gt;" }, { ~r("), "\\&quot;" }]
    Enum.reduce escape_map, binary, fn({ re, escape }, acc) -> Regex.replace(re, acc, escape) end
  end

  # Get the breadcrumbs HTML.
  #
  # If module is :overview generates the breadcrumbs for the overview.
  defp module_breadcrumbs(config, modules, module) do
    parts = [root_breadcrumbs(config), { "Overview", "overview.html" }]
    aliases = Module.split(module.module)
    modules = Enum.map(modules, &(&1.module))

    { crumbs, _ } =
      Enum.map_reduce(aliases, [], fn item, parents ->
        path = parents ++ [item]
        mod  = Module.concat(path)
        page = if mod in modules, do: inspect(mod) <> ".html"
        { { item, page }, path }
      end)

    generate_breadcrumbs(parts ++ crumbs)
  end

  defp page_breadcrumbs(config, title, link) do
    generate_breadcrumbs [root_breadcrumbs(config), { title, link }]
  end

  defp root_breadcrumbs(config) do
    { "#{config.project} v#{config.version}", nil }
  end

  defp generate_breadcrumbs(crumbs) do
    Enum.map_join(crumbs, " &rarr; ", fn { name, ref } ->
      if ref, do: "<a href=\"#{h(ref)}\">#{h(name)}</a>", else: h(name)
    end)
  end

  templates = [
    index_template: [:config],
    list_template: [:scope, :nodes, :config, :has_readme],
    overview_template: [:config, :modules, :exceptions, :protocols],
    module_template: [:config, :module, :types, :functions, :macros, :callbacks, :all],
    readme_template: [:config, :content],
    list_item_template: [:node],
    overview_entry_template: [:node],
    summary_template: [:node],
    detail_template: [:node, :_module],
    type_detail_template: [:node, :_module],
  ]

  Enum.each templates, fn({ name, args }) ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    EEx.function_from_file :def, name, filename, args
  end
end
