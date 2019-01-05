defmodule ExDoc.Formatter.HTML.Templates do
  @moduledoc false
  require EEx

  alias ExDoc.Formatter.HTML

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(module_node, nodes_map, config) do
    summary = module_summary(module_node)
    module_template(config, module_node, summary, nodes_map)
  end

  @doc """
  Get the full specs from a function, already in HTML form.
  """
  def get_specs(%ExDoc.TypeNode{spec: spec}) do
    [spec]
  end

  def get_specs(%ExDoc.FunctionNode{specs: specs}) when is_list(specs) do
    presence(specs)
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
  Get the pretty name of a function node
  """
  def pretty_type(%{type: t}) do
    Atom.to_string(t)
  end

  @doc """
  Generate a link id
  """
  def link_id(node), do: link_id(node.id, node.type)

  def link_id(id, type) do
    case type do
      :macrocallback -> "c:#{id}"
      :callback -> "c:#{id}"
      :type -> "t:#{id}"
      :opaque -> "t:#{id}"
      _ -> "#{id}"
    end
  end

  @doc """
  Generate a link title
  """
  def link_title(module, node) do
    mfa_without_prefix = "#{module.id}.#{node.id}"
    link_id(mfa_without_prefix, node.type)
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
  surrounding white-spaces and traling `:`.

  If `doc` is `nil`, it returns `nil`.
  """
  @spec synopsis(String.t()) :: String.t()
  @spec synopsis(nil) :: nil
  def synopsis(nil), do: nil

  def synopsis(doc) when is_binary(doc) do
    case :binary.split(doc, "</p>") do
      [left, _] -> String.trim_trailing(left, ":") <> "</p>"
      [all] -> all
    end
  end

  defp presence([]), do: nil
  defp presence(other), do: other

  @doc false
  def h(binary) do
    escape_map = [{"&", "&amp;"}, {"<", "&lt;"}, {">", "&gt;"}, {~S("), "&quot;"}]

    Enum.reduce(escape_map, binary, fn {pattern, escape}, acc ->
      String.replace(acc, pattern, escape)
    end)
  end

  @doc false
  def enc_h(binary) do
    binary
    |> URI.encode()
    |> h()
  end

  defp is_enc?(binary) do
    h(binary) != enc_h(binary)
  end

  @doc """
  Create a JS object which holds all the items displayed in the sidebar area
  """
  def create_sidebar_items(nodes_map, extras) do
    nodes_map = [sidebar_items_extras(extras) | Enum.map(nodes_map, &sidebar_items_keys/1)]
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
      |> Enum.map_join(",", fn {header, anchor} ->
        sidebar_items_object(header, anchor, hyphenize(id, title))
      end)

    ~s/{"id":"#{id}","title":"#{title}","group":"#{group}","headers":[#{headers}]}/
  end

  defp hyphenize(id, title) do
    [id, title]
    |> Enum.filter(&(&1 != ""))
    |> Enum.filter(&(&1 != nil))
    |> Enum.join(" — ")
  end

  @h2_regex ~r/<h2.*?>(.*?)<\/h2>/m
  defp extract_headers(content) do
    @h2_regex
    |> Regex.scan(content, capture: :all_but_first)
    |> List.flatten()
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&HTML.strip_tags/1)
    |> Enum.map(&{&1, HTML.text_to_id(&1)})
  end

  defp sidebar_items_node(module_node) do
    sidebar_items_by_group_with_module = fn group ->
      sidebar_items_by_group(module_node, group)
    end

    items =
      module_node
      |> module_summary()
      |> Enum.reject(fn {_type, nodes_map} -> nodes_map == [] end)
      |> Enum.map_join(",", fn group -> sidebar_items_by_group_with_module.(group) end)

    sidebar_items_json_string(module_node, items)
  end

  defp sidebar_items_by_group(module_node, {group, docs}) do
    objects =
      Enum.map_join(docs, ",", fn doc ->
        sidebar_items_object(doc.id, HTML.link_id(doc), link_title(module_node, doc)
      end)

    ~s/{"key":"#{HTML.text_to_id(group)}","name":"#{group}","nodes":[#{objects}]}/
  end

  defp sidebar_items_object(id, anchor, title) do
    ~s/{"id":"#{id}","anchor":"#{URI.encode(anchor)}","link_title":"#{title}"}/
  end

  defp sidebar_items_json_string(module_node, items) do
    json_attrs =
      for key <- [:id, :title, :nested_title, :nested_context],
          value = Map.get(module_node, key),
          do: [json_kv(key, inspect(value)), ?,]

    json_attrs = [json_attrs | ~s("group":"#{module_node.group}")]

    json_attrs =
      case items do
        "" -> json_attrs
        items -> [json_attrs, ?, | json_kv(:nodeGroups, "[#{items}]")]
      end

    IO.iodata_to_binary([?{, json_attrs, ?}])
  end

  defp json_kv(key, value), do: [?", Atom.to_string(key), ?", ?:, value]

  def module_summary(module_node) do
    [Types: module_node.typespecs] ++
      function_groups(module_node.function_groups, module_node.docs) ++
      [Callbacks: Enum.filter(module_node.docs, &(&1.type in [:callback, :macrocallback]))]
  end

  defp function_groups(groups, docs) do
    for group <- groups, do: {group, Enum.filter(docs, &(&1.group == group))}
  end

  defp logo_path(%{logo: nil}), do: nil
  defp logo_path(%{logo: logo}), do: "assets/logo#{Path.extname(logo)}"

  defp sidebar_type(:exception), do: "exceptions"
  defp sidebar_type(:extra), do: "extras"
  defp sidebar_type(:module), do: "modules"
  defp sidebar_type(:behaviour), do: "modules"
  defp sidebar_type(:protocol), do: "modules"
  defp sidebar_type(:task), do: "tasks"
  defp sidebar_type(:search), do: "search"

  def asset_rev(output, pattern) do
    output = Path.expand(output)

    output
    |> Path.join(pattern)
    |> Path.wildcard()
    |> relative_asset(output, pattern)
  end

  defp relative_asset([], output, pattern),
    do: raise("could not find matching #{output}/#{pattern}")

  defp relative_asset([h | _], output, _pattern), do: Path.relative_to(h, output)

  @doc """
  Link headings found with `regex` with in the given `content`. IDs are
  prefixed with `prefix`.
  """
  @heading_regex ~r/<(h[23]).*?>(.*?)<\/\1>/m
  @spec link_headings(String.t(), Regex.t(), String.t(), String.t()) :: String.t()
  def link_headings(content, regex \\ @heading_regex, prefix \\ "", link_title \\ "")
  def link_headings(nil, _, _, _), do: nil

  def link_headings(content, regex, prefix, link_title) do
    Regex.replace(regex, content, fn match, tag, title ->
      link_heading(match, tag, title, HTML.text_to_id(title), prefix, link_title)
    end)
  end

  defp link_heading(match, _tag, _title, "", _prefix, _link_title), do: match

  defp link_heading(_match, tag, title, id, prefix, link_title) do
    link_title = "Link to #{link_title} #{HTML.strip_tags(title)}"

    """
    <#{tag} id="#{prefix}#{id}" class="section-heading">
      <a href="##{prefix}#{id}" class="hover-link" title="#{link_title}"><span class="icon-link" aria-hidden="true"></span></a>
      #{title}
    </#{tag}>
    """
  end

  defp link_moduledoc_headings(content, link_title) do
    link_headings(content, @heading_regex, "module-", link_title)
  end

  defp link_detail_headings(content, prefix, link_title) do
    link_headings(content, @heading_regex, prefix <> "-", link_title)
  end

  defp link_extras_headings(content, page_title) do
    link_headings(content, @heading_regex, "", page_title)
  end

  templates = [
    detail_template: [:node, :module],
    footer_template: [:config],
    head_template: [:config, :page],
    module_template: [:config, :module, :summary, :nodes_map],
    not_found_template: [:config, :nodes_map],
    api_reference_entry_template: [:module_node],
    api_reference_template: [:config, :nodes_map],
    extra_template: [:config, :title, :nodes_map, :content],
    search_template: [:config, :nodes_map],
    sidebar_template: [:config, :nodes_map],
    summary_template: [:name, :nodes, :module],
    summary_entry_template: [:node, :module],
    redirect_template: [:config, :redirect_to]
  ]

  Enum.each(templates, fn {name, args} ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    @doc false
    EEx.function_from_file(:def, name, filename, args, trim: true)
  end)
end
