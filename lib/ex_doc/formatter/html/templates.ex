defmodule ExDoc.Formatter.HTML.Templates do
  @moduledoc """
  Handle all template interfaces for the HTML formatter.
  """

  require EEx

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(module_node, modules, exceptions, protocols, config) do
    types = group_types(module_node)
    module_template(config, module_node, types.types, types.functions, types.macros, types.callbacks,
                    modules, exceptions, protocols)
  end

  @doc """
  Get the full specs from a function, already in HTML form.
  """
  def get_specs(%ExDoc.TypeNode{spec: spec}) do
    [spec]
  end
  def get_specs(%ExDoc.FunctionNode{specs: specs}) when is_list(specs) do
    presence specs
  end
  def get_specs(_node) do
    nil
  end

  @doc """
  Get defaults clauses.
  """
  def get_defaults(%{defaults: defaults}) do
    defaults
  end
  def get_defaults(_) do
    []
  end

  @doc """
  Converts markdown to HTML using the given node file+line.
  """
  def to_html(nil, %{source_path: _, doc_line: _}) do
    nil
  end
  def to_html(doc, %{source_path: file, doc_line: line}) when is_binary(doc) do
    ExDoc.Markdown.to_html(doc, file: file, line: line + 1)
  end

  @doc """
  Get the pretty name of a function node
  """
  def pretty_type(%ExDoc.TypeNode{type: t}) do
    Atom.to_string(t)
  end
  def pretty_type(%ExDoc.FunctionNode{type: t}) do
    case t do
      :def           -> "function"
      :defmacro      -> "macro"
      :callback      -> "callback"
      :macrocallback -> "macro callback"
    end
  end

  @doc """
  Generate a link id
  """
  def link_id(module_node), do: link_id(module_node.id, module_node.type)
  def link_id(id, type) do
    case type do
      :macrocallback -> "c:#{id}"
      :callback      -> "c:#{id}"
      :type          -> "t:#{id}"
      :opaque        -> "t:#{id}"
      _              -> "#{id}"
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

  defp sidebar_items_extra({id, title, group, headers}) do
    headers = Enum.map_join(headers, ",", fn {header, anchor} ->
      sidebar_items_object(header, anchor)
    end)
    ~s/{"id":"#{id}","title":"#{title}","group":"#{group}","headers":[#{headers}]}/
  end

  defp sidebar_items_node(module_node) do
    items =
      module_node
      |> group_types()
      |> Enum.reject(fn {_type, entries} -> entries == [] end)
      |> Enum.map_join(",", &sidebar_items_by_type/1)

    if items == "" do
      ~s/{"id":"#{module_node.id}","title":"#{module_node.id}"}/
    else
      ~s/{"id":"#{module_node.id}","title":"#{module_node.id}",#{items}}/
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

  def group_types(module_node) do
    %{types: module_node.typespecs,
      functions: Enum.filter(module_node.docs, & &1.type in [:def]),
      macros: Enum.filter(module_node.docs, & &1.type in [:defmacro]),
      callbacks: Enum.filter(module_node.docs, & &1.type in [:callback, :macrocallback])}
  end

  defp logo_path(%{logo: nil}), do: nil
  defp logo_path(%{logo: logo}), do: "assets/logo#{Path.extname(logo)}"

  defp sidebar_type(:protocol), do: "protocols"
  defp sidebar_type(:exception), do: "exceptions"
  defp sidebar_type(:extra), do: "extras"
  defp sidebar_type(:module), do: "modules"
  defp sidebar_type(:behaviour), do: "modules"

  def asset_rev(output, pattern) do
    output = Path.expand(output)

    output
    |> Path.join(pattern)
    |> Path.wildcard()
    |> relative_asset(output)
  end

  defp relative_asset([], _), do: nil
  defp relative_asset([h|_], output), do: Path.relative_to(h, output)

  @doc """
  Extract a linkable ID from a heading
  """
  @spec header_to_id(String.t) :: String.t
  def header_to_id(header) do
    header
    |> String.replace(~r/<.+>/, "")
    |> String.replace(~r/&#\d+;/, "")
    |> String.replace(~r/&[A-Za-z0-9]+;/, "")
    |> String.replace(~r/\W+/u, "-")
    |> String.strip(?-)
    |> String.downcase()
  end

  @doc """
  Link secondary headings found with `regex` with in the given `content`.
  IDs are prefixed with `prefix`.
  """
  @h2_regex ~r/<h2.*?>(.+)<\/h2>/m

  @spec link_headings(String.t, Regex.t, String.t) :: String.t
  def link_headings(content, regex \\ @h2_regex, prefix \\ "")
  def link_headings(nil, _, _), do: nil
  def link_headings(content, regex, prefix) do
    Regex.replace(regex, content, fn match, title ->
      link_heading(match, title, header_to_id(title), prefix)
    end)
  end

  defp link_heading(match, _title, "", _prefix), do: match
  defp link_heading(_match, title, id, prefix) do
    """
    <h2 id="#{prefix}#{id}" class="section-heading">
      <a href="##{prefix}#{id}" class="hover-link"><i class="icon-link"></i></a>
      #{title}
    </h2>
    """
  end

  defp link_moduledoc_headings(content) do
    link_headings(content, @h2_regex, "module-")
  end

  defp link_detail_headings(content, prefix) do
    link_headings(content, @h2_regex, prefix <> "-")
  end

  templates = [
    detail_template: [:module_node, :_module],
    footer_template: [:config],
    head_template: [:config, :page],
    module_template: [:config, :module, :types, :functions, :macros, :callbacks,
                      :modules, :exceptions, :protocols],
    not_found_template: [:config, :modules, :exceptions, :protocols],
    api_reference_entry_template: [:module_node],
    api_reference_template: [:config, :modules, :exceptions, :protocols],
    extra_template: [:config, :title, :modules, :exceptions, :protocols, :content],
    sidebar_template: [:config, :modules, :exceptions, :protocols],
    summary_template: [:name, :nodes],
    summary_item_template: [:module_node],
    redirect_template: [:config, :redirect_to],
  ]

  Enum.each templates, fn({name, args}) ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    @doc false
    EEx.function_from_file :def, name, filename, args
  end
end
