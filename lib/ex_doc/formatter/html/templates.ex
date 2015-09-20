defmodule ExDoc.Formatter.HTML.Templates do
  @moduledoc """
  Handle all template interfaces for the HTML formatter.
  """

  require EEx
  require ExDoc

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(node, modules, exceptions, protocols, config) do
    types = group_types(node)
    module_template(config, node, types.types, types.functions, types.macros, types.callbacks,
                    modules, exceptions, protocols)
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
      :def           -> "function"
      :defmacro      -> "macro"
      :callback      -> "callback"
      :macrocallback -> "macro callback"
      :type          -> "type"
    end
  end

  # Generate a link id
  defp link_id(node), do: link_id(node.id, node.type)
  defp link_id(id, type) do
    case type do
      :macrocallback -> "c:#{id}"
      :callback      -> "c:#{id}"
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

  @doc false
  def h(binary) do
    escape_map = [{"&", "&amp;"}, {"<", "&lt;"}, {">", "&gt;"}, {"\"", "&quot;"}]
    Enum.reduce escape_map, binary, fn({pattern, escape}, acc) ->
      String.replace(acc, pattern, escape)
    end
  end

  @doc false
  def enc_h(binary) do
    URI.encode(binary) |> h
  end

  @spec create_sidebar_items(list) :: String.t
  def create_sidebar_items(input) do
    object =
      input
      |> Enum.into([], &sidebar_items_keys/1)
      |> Enum.join(",")
    "sidebarNodes={#{object}}"
  end

  defp sidebar_items_keys({:extras, value}) do
    keys =
      value
      |> Enum.into([], &sidebar_items_extra/1)
      |> Enum.join(",")
    ~s/"extras":[#{keys}]/
  end

  defp sidebar_items_keys({id, value}) do
    keys =
      value
      |> Enum.into([], &sidebar_items_node/1)
      |> Enum.join(",")
    ~s/"#{id}":[#{keys}]/
  end

  defp sidebar_items_extra({id, title, headers}) do
    headers = Enum.map_join(headers, ",", fn {header, anchor} ->
      sidebar_items_object(header, anchor)
    end)
    ~s/{"id":"#{id}","title":"#{title}","headers":[#{headers}]}/
  end

  defp sidebar_items_node(node) do
    if Enum.empty?(node.docs) do
      ~s/{"id":"#{node.id}","title":"#{node.id}"}/
    else
      types =
        group_types(node)
        |> Enum.reject(fn {_type, entries} -> entries == [] end)
        |> Enum.map_join(",", &sidebar_items_by_type/1)
      ~s/{"id":"#{node.id}","title":"#{node.id}",#{types}}/
    end
  end

  defp sidebar_items_by_type({type, docs}) do
    objects = Enum.map_join(docs, ",", fn doc ->
      sidebar_items_object(doc.id, link_id(doc))
    end)
    ~s/"#{type}":[#{objects}]/
  end

  defp sidebar_items_object(id, anchor) do
    ~s/{"id":"#{id}","anchor":"#{anchor}"}/
  end

  defp group_types(node) do
    %{types: node.typespecs,
      functions: Enum.filter(node.docs, & &1.type in [:def]),
      macros: Enum.filter(node.docs, & &1.type in [:defmacro]),
      callbacks: Enum.filter(node.docs, & &1.type in [:callback, :macrocallback])}
  end

  defp logo_path(%{logo: nil}), do: nil
  defp logo_path(%{logo: logo}), do: "assets/logo#{Path.extname(logo)}"

  defp sidebar_type(:protocol), do: "protocols"
  defp sidebar_type(:exception), do: "exceptions"
  defp sidebar_type(:extra), do: "extras"
  defp sidebar_type(:module), do: "modules"
  defp sidebar_type(:behaviour), do: "modules"

  templates = [
    detail_template: [:node, :_module],
    footer_template: [],
    head_template: [:config, :page],
    module_template: [:config, :module, :types, :functions, :macros, :callbacks,
                      :modules, :exceptions, :protocols],
    not_found_template: [:config, :modules, :exceptions, :protocols],
    api_reference_entry_template: [:node],
    api_reference_template: [:config, :modules, :exceptions, :protocols],
    extra_template: [:config, :title, :modules, :exceptions, :protocols, :content],
    sidebar_template: [:config, :modules, :exceptions, :protocols],
    summary_template: [:name, :nodes],
    summary_item_template: [:node],
    type_detail_template: [:node, :_module],
    redirect_template: [:config, :redirect_to],
  ]

  Enum.each templates, fn({ name, args }) ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    EEx.function_from_file :def, name, filename, args
  end
end
