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
    functions   = Enum.filter node.docs, & &1.type in [:def]
    macros      = Enum.filter node.docs, & &1.type in [:defmacro]
    callbacks   = Enum.filter node.docs, & &1.type in [:defcallback, :defmacrocallback]
    module_template(config, node, types, functions, macros, callbacks, all)
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

  defp presence([]),    do: nil
  defp presence(other), do: other

  defp h(binary) do
    escape_map = [{"&", "&amp;"}, {"<", "&lt;"}, {">", "&gt;"}, {"\"", "&quot;"}]
    Enum.reduce escape_map, binary, fn({pattern, escape}, acc) ->
      String.replace(acc, pattern, escape)
    end
  end

  defp sidebar_items_object(node) do
    ~s{"id":"#{node.id}","anchor":"#{h link_id(node)}"}
  end

  defp sidebar_items_by_type({type, node}) do
    objects = node |> Enum.map_join("},{", &(sidebar_items_object(&1)))
    ~s/"#{type}":[{#{objects}}]/
  end

  defp sidebar_items_entry(node) do
    result = ~s/"id":"#{node.id}"/
    unless Enum.empty?(node.docs) do
      types = [types: node.typespecs,
        functions: Enum.filter(node.docs, & &1.type in [:def]),
        macros: Enum.filter(node.docs, & &1.type in [:defmacro]),
        callbacks: Enum.filter(node.docs, & &1.type in [:defcallback, :defmacrocallback])]
      |> Enum.reject(fn {_type, entries} -> entries == [] end)
      |> Enum.map_join(",", &sidebar_items_by_type(&1))
      result = "#{result},#{types}"
    end
    result
  end

  defp sidebar_items_keys(node) do
    keys = Enum.into(node.value, [], &(sidebar_items_entry(&1)))
           |> Enum.join("},{")
    ~s/"#{node.id}":[{#{keys}}]/
  end

  @spec create_sidebar_items(list) :: String.t
  def create_sidebar_items(input) do
    object = Enum.into(input, [], &(sidebar_items_keys(&1)))
             |> Enum.join(",")
    "sidebarNodes={#{object}}"
  end

  defp logo_path(%{logo: nil}), do: nil
  defp logo_path(%{logo: logo}), do: "assets/logo#{Path.extname(logo)}"

  templates = [
    detail_template: [:node, :_module],
    footer_template: [],
    head_template: [:config, :page],
    module_template: [:config, :module, :types, :functions, :macros, :callbacks, :all],
    not_found_template: [:config, :modules, :exceptions, :protocols],
    overview_entry_template: [:node],
    overview_template: [:config, :modules, :exceptions, :protocols],
    readme_template: [:config, :modules, :exceptions, :protocols, :content],
    sidebar_template: [:config, :modules, :exceptions, :protocols],
    summary_template: [:node],
    type_detail_template: [:node, :_module],
    redirect_template: [:config, :redirect_to],
  ]

  Enum.each templates, fn({ name, args }) ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    EEx.function_from_file :def, name, filename, args
  end
end
