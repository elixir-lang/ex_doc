defmodule ExDoc.Formatter.HTML.Templates do
  @moduledoc """
  Handle all template interfaces for the HTML formatter.
  """

  require EEx

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

  @doc """
  Gets the first paragraph of the documentation of a node. It strips
  surrounding spaces and strips traling `:` and `.`.

  If `doc` is `nil`, it returns `nil`.
  """
  @spec synopsis(String.t) :: String.t
  @spec synopsis(nil) :: nil

  def synopsis(nil), do: nil
  def synopsis(""),  do: ""
  def synopsis(doc) when is_bitstring(doc) do
    doc
    |> String.split(~r/\n\s*\n/)
    |> hd()
    |> String.strip()
    |> String.replace(~r{[.:\s]+$}, "")
    |> String.rstrip()
  end

  defp presence([]),    do: nil
  defp presence(other), do: other

  @doc false
  def h(binary) do
    escape_map = [{"&", "&amp;"}, {"<", "&lt;"}, {">", "&gt;"}, {~S("), "&quot;"}]
    Enum.reduce escape_map, binary, fn({pattern, escape}, acc) ->
      String.replace(acc, pattern, escape)
    end
  end

  @doc false
  def enc_h(binary) do
    binary
    |> URI.encode()
    |> h()
  end

  @doc """
  Create a JS object which holds all the items displayed in the sidebar area
  """
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
        node
        |> group_types()
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
    ~s/{"id":"#{id}","anchor":"#{URI.encode(anchor)}"}/
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

  defp asset_rev(output, pattern) do
    output = Path.expand(output)

    output
    |> Path.join(pattern)
    |> Path.wildcard()
    |> relative_asset(output)
  end

  defp relative_asset([], _), do: nil
  defp relative_asset([h|_], output), do: Path.relative_to(h, output)

  @doc """
  Extract a linkable id from a heading
  """
  @spec header_to_id(String.t) :: String.t
  def header_to_id(header) do
    header
    |> String.replace(~r/\W+/u, "-")
    |> String.strip(?-)
    |> String.downcase()
    |> h()
  end

  @h2_regex ~r/<h2.*?>(.+)<\/h2>/m
  defp link_moduledoc_headings(content) do
    Regex.replace(@h2_regex, content, fn match, title ->
      id = header_to_id(title)
      if id == "" do
        match
      else
        """
        <h2 id="module-#{id}" class="section-heading">
          <a class="hover-link" href="#module-#{id}">
            <i class="icon-link"></i>
          </a>
          #{title}
        </h2>
        """
      end
    end)
  end

  templates = [
    detail_template: [:node, :_module],
    footer_template: [:config],
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
    @doc false
    EEx.function_from_file :def, name, filename, args
  end
end
