defmodule ExDoc.Formatter.HTML.Templates do
  @moduledoc """
  Handle all template interfaces for the HTML formatter.
  """

  require EEx

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(node, config, all, has_readme) do
    types       = node.typespecs
    functions   = Enum.filter node.docs, & &1.type in [:def]
    macros      = Enum.filter node.docs, & &1.type in [:defmacro]
    callbacks   = Enum.filter node.docs, & &1.type in [:defcallback, :defmacrocallback]
    module_template(config, node, types, functions, macros, callbacks, all, has_readme)
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
      :def              -> "function"
      :defmacro         -> "macro"
      :defcallback      -> "callback"
      :defmacrocallback -> "macro callback"
      :type             -> "type"
    end
  end

  # Generate a link id
  defp link_id(node), do: link_id(node.id, node.type)
  defp link_id(id, type) do
    case type do
      :defmacrocallback -> "c:#{id}"
      :defcallback      -> "c:#{id}"
      :type             -> "t:#{id}"
      _                 -> "#{id}"
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
    escape_map = [{"&", "&amp;"}, {"<", "&lt;"}, {">", "&gt;"}, {"\"", "&quot;"}]
    Enum.reduce escape_map, binary, fn({pattern, escape}, acc) ->
      String.replace(acc, pattern, escape)
    end
  end

  # Get the breadcrumbs HTML.
  #
  # If module is :overview generates the breadcrumbs for the overview.
  defp module_breadcrumbs(config, modules, module) do
    parts = [root_breadcrumbs(config), {"Overview", "overview.html"}]
    aliases = Module.split(module.module)
    modules = Enum.map(modules, &(&1.module))

    {crumbs, _} =
      Enum.map_reduce(aliases, [], fn item, parents ->
        path = parents ++ [item]
        mod  = Module.concat(path)
        page = if mod in modules, do: inspect(mod) <> ".html#content"
        {{item, page}, path}
      end)

    generate_breadcrumbs(parts ++ crumbs)
  end

  defp page_breadcrumbs(config, title, link) do
    generate_breadcrumbs [root_breadcrumbs(config), { title, link }]
  end

  defp root_breadcrumbs(config) do
    {"#{config.project} v#{config.version}", nil}
  end

  defp generate_breadcrumbs(crumbs) do
    Enum.map_join(crumbs, " &rarr; ", fn { name, ref } ->
      if ref, do: "<a href=\"#{h(ref)}\">#{h(name)}</a>", else: h(name)
    end)
  end

  templates = [
    detail_template: [:node, :_module],
    footer_template: [],
    head_template: [:config, :page],
    module_template: [:config, :module, :types, :functions, :macros, :callbacks, :all, :has_readme],
    overview_entry_template: [:node],
    overview_template: [:config, :modules, :exceptions, :protocols, :has_readme],
    readme_template: [:config, :modules, :exceptions, :protocols, :content],
    sidebar_items_entry_template: [:node],
    sidebar_items_keys_template: [:node],
    sidebar_items_template: [:input],
    sidebar_template: [:config, :modules, :exceptions, :protocols, :has_readme],
    summary_template: [:node],
    type_detail_template: [:node, :_module],
    redirect_template: [:config, :redirect_to],
  ]

  Enum.each templates, fn({ name, args }) ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    EEx.function_from_file :def, name, filename, args
  end
end
