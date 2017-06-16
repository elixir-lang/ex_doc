defmodule ExDoc.Formatter.HTML.Templates do
  @moduledoc false
  require EEx

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(module_node, nodes_map, config) do
    summary_map = group_summary(module_node)
    module_template(config, module_node, summary_map, nodes_map)
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
  Returns the HTML formatted title for the module page.
  """
  def module_title(%{type: :task, title: title}),
    do: "mix " <> title
  def module_title(%{type: :module, title: title}),
    do: title
  def module_title(%{type: type, title: title}),
    do: title <> " <small>#{type}</small>"

  @doc """
  Gets the first paragraph of the documentation of a node. It strips
  surrounding spaces and strips traling `:` and `.`.

  If `doc` is `nil`, it returns `nil`.
  """
  @spec synopsis(String.t) :: String.t
  @spec synopsis(nil) :: nil

  def synopsis(nil), do: nil
  def synopsis(""),  do: ""
  def synopsis(doc) when is_binary(doc) do
    doc
    |> String.split(~r/\n\s*\n/)
    |> hd()
    |> String.trim()
    |> String.replace(~r{[.:\s]+$}, "")
    |> String.trim_trailing()
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
  def create_sidebar_items(nodes_map, extras) do
    nodes_map =
      [sidebar_items_extras(extras) | Enum.map(nodes_map, &sidebar_items_keys/1)]
    "sidebarNodes={#{Enum.join(nodes_map, ",")}}"
  end

  defp sidebar_items_extras(extras) do
    keys =
      extras
      |> Enum.map(&sidebar_items_extra/1)
      |> Enum.join(",")
    ~s/"extras":[#{keys}]/
  end

  defp sidebar_items_keys({id, value}) do
    keys =
      value
      |> Enum.map(&sidebar_items_node/1)
      |> Enum.join(",")
    ~s/"#{id}":[#{keys}]/
  end

  defp sidebar_items_extra(%{id: id, title: title, group: group, content: content}) do
    headers =
      content
      |> extract_headers
      |> Enum.map_join(",", fn {header, anchor} -> sidebar_items_object(header, anchor) end)
    ~s/{"id":"#{id}","title":"#{title}","group":"#{group}","headers":[#{headers}]}/
  end

  @h2_regex ~r/<h2.*?>(.*?)<\/h2>/m
  @clean_html_regex ~r/<(?:[^>=]|='[^']*'|="[^"]*"|=[^'"][^\s>]*)*>/
  defp extract_headers(content) do
    @h2_regex
    |> Regex.scan(content, capture: :all_but_first)
    |> List.flatten()
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&(String.replace(&1, @clean_html_regex, "")))
    |> Enum.map(&{&1, header_to_id(&1)})
  end

  defp sidebar_items_node(module_node) do
    items =
      module_node
      |> group_summary()
      |> Enum.reject(fn {_type, nodes_map} -> nodes_map == [] end)
      |> Enum.map_join(",", &sidebar_items_by_type/1)

    if items == "" do
      ~s/{"id":"#{module_node.id}","title":"#{module_node.title}"}/
    else
      ~s/{"id":"#{module_node.id}","title":"#{module_node.title}",#{items}}/
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

  def group_summary(module_node) do
    %{types: module_node.typespecs,
      functions: Enum.filter(module_node.docs, & &1.type in [:def, :defmacro]),
      callbacks: Enum.filter(module_node.docs, & &1.type in [:callback, :macrocallback])}
  end

  defp logo_path(%{logo: nil}), do: nil
  defp logo_path(%{logo: logo}), do: "assets/logo#{Path.extname(logo)}"

  defp sidebar_type(:protocol), do: "protocols"
  defp sidebar_type(:exception), do: "exceptions"
  defp sidebar_type(:extra), do: "extras"
  defp sidebar_type(:module), do: "modules"
  defp sidebar_type(:behaviour), do: "modules"
  defp sidebar_type(:task), do: "tasks"

  def asset_rev(output, pattern) do
    output = Path.expand(output)

    output
    |> Path.join(pattern)
    |> Path.wildcard()
    |> relative_asset(output, pattern)
  end

  defp relative_asset([], output, pattern), do: raise "could not find matching #{output}/#{pattern}"
  defp relative_asset([h|_], output, _pattern), do: Path.relative_to(h, output)

  @doc """
  Extract a linkable ID from a heading
  """
  @spec header_to_id(String.t) :: String.t
  def header_to_id(header) do
    header
    |> String.replace(@clean_html_regex, "")
    |> String.replace(~r/&#\d+;/, "")
    |> String.replace(~r/&[A-Za-z0-9]+;/, "")
    |> String.replace(~r/\W+/u, "-")
    |> String.trim("-")
    |> String.downcase()
  end

  @doc """
  Link headings found with `regex` with in the given `content`. IDs are
  prefixed with `prefix`.
  """
  @heading_regex ~r/<(h[23]).*?>(.*?)<\/\1>/m
  @spec link_headings(String.t, Regex.t, String.t) :: String.t
  def link_headings(content, regex \\ @heading_regex, prefix \\ "")
  def link_headings(nil, _, _), do: nil
  def link_headings(content, regex, prefix) do
    Regex.replace(regex, content, fn match, tag, title ->
      link_heading(match, tag, title, header_to_id(title), prefix)
    end)
  end

  defp link_heading(match, _tag, _title, "", _prefix), do: match
  defp link_heading(_match, tag, title, id, prefix) do
    """
    <#{tag} id="#{prefix}#{id}" class="section-heading">
      <a href="##{prefix}#{id}" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>
      #{title}
    </#{tag}>
    """
  end

  defp link_moduledoc_headings(content) do
    link_headings(content, @heading_regex, "module-")
  end

  defp link_detail_headings(content, prefix) do
    link_headings(content, @heading_regex, prefix <> "-")
  end

  templates = [
    detail_template: [:module_node, :_module],
    footer_template: [:config],
    head_template: [:config, :page],
    module_template: [:config, :module, :summary_map, :nodes_map],
    not_found_template: [:config, :nodes_map],
    api_reference_entry_template: [:module_node],
    api_reference_template: [:config, :nodes_map],
    extra_template: [:config, :title, :nodes_map, :content],
    sidebar_template: [:config, :nodes_map],
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
