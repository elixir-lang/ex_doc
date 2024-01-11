defmodule ExDoc.Formatter.HTML.Templates do
  @moduledoc false
  require EEx

  import ExDoc.Utils,
    only: [
      h: 1,
      before_closing_body_tag: 2,
      before_closing_footer_tag: 2,
      before_closing_head_tag: 2
    ]

  # TODO: It should not depend on the parent module. Move required HTML functions to Utils.
  # TODO: Add tests that assert on the returned structured, not on JSON
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
  Format the attribute type used to define the spec of the given `node`.
  """
  def format_spec_attribute(module, node) do
    module.language.format_spec_attribute(node)
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
  Returns the HTML formatted title for the module page.
  """
  def module_type(%{type: :task}), do: ""
  def module_type(%{type: :module}), do: ""
  def module_type(%{type: type}), do: "<small>#{type}</small>"

  @doc """
  Gets the first paragraph of the documentation of a node. It strips
  surrounding white-spaces and trailing `:`.

  If `doc` is `nil`, it returns `nil`.
  """
  @spec synopsis(String.t()) :: String.t()
  @spec synopsis(nil) :: nil
  def synopsis(nil), do: nil

  def synopsis(doc) when is_binary(doc) do
    doc =
      case :binary.split(doc, "</p>") do
        [left, _] -> String.trim_trailing(left, ":") <> "</p>"
        [all] -> all
      end

    # Remove any anchors found in synopsis.
    # Old Erlang docs placed anchors at the top of the documentation
    # for links. Ideally they would have been removed but meanwhile
    # it is simpler to guarantee they won't be duplicated in docs.
    Regex.replace(~r|(<[^>]*) id="[^"]*"([^>]*>)|, doc, ~S"\1\2", [])
  end

  defp presence([]), do: nil
  defp presence(other), do: other

  defp enc(binary), do: URI.encode(binary)

  @doc """
  Create a JS object which holds all the items displayed in the sidebar area
  """
  def create_sidebar_items(nodes_map, extras) do
    nodes =
      nodes_map
      |> Enum.map(&sidebar_module/1)
      |> Map.new()
      |> Map.put(:extras, sidebar_extras(extras))

    ["sidebarNodes=" | ExDoc.Utils.to_json(nodes)]
  end

  defp sidebar_extras(extras) do
    for extra <- extras do
      %{id: id, title: title, group: group, content: content} = extra

      %{
        id: to_string(id),
        title: to_string(title),
        group: to_string(group),
        headers: extract_headers(content)
      }
    end
  end

  defp sidebar_module({id, modules}) do
    modules =
      for module <- modules do
        extra =
          module
          |> module_summary()
          |> case do
            [] -> []
            entries -> [nodeGroups: Enum.map(entries, &sidebar_entries/1)]
          end

        sections = module_sections(module)

        deprecated? = not is_nil(module.deprecated)

        pairs =
          for key <- [:id, :title, :nested_title, :nested_context],
              value = Map.get(module, key),
              do: {key, value}

        pairs = [{:deprecated, deprecated?} | pairs]

        Map.new([group: to_string(module.group)] ++ extra ++ pairs ++ sections)
      end

    {id, modules}
  end

  defp sidebar_entries({group, nodes}) do
    nodes =
      for node <- nodes do
        id =
          if "struct" in node.annotations do
            node.signature
          else
            if node.name == nil do
              "nil/#{node.arity}"
            else
              "#{node.name}/#{node.arity}"
            end
          end

        deprecated? = not is_nil(node.deprecated)

        %{id: id, title: node.signature, anchor: URI.encode(node.id), deprecated: deprecated?}
      end

    %{key: HTML.text_to_id(group), name: group, nodes: nodes}
  end

  defp module_sections(%ExDoc.ModuleNode{rendered_doc: nil}), do: [sections: []]

  defp module_sections(module) do
    {sections, _} =
      module.rendered_doc
      |> extract_headers()
      |> Enum.map_reduce(%{}, fn header, acc ->
        # TODO Duplicates some of the logic of link_headings/3
        case Map.fetch(acc, header.id) do
          {:ok, id} ->
            {%{header | anchor: "module-#{header.anchor}-#{id}"}, Map.put(acc, header.id, id + 1)}

          :error ->
            {%{header | anchor: "module-#{header.anchor}"}, Map.put(acc, header.id, 1)}
        end
      end)

    [sections: sections]
  end

  # TODO: split into sections in Formatter.HTML instead.
  @h2_regex ~r/<h2.*?>(.*?)<\/h2>/m
  defp extract_headers(content) do
    @h2_regex
    |> Regex.scan(content, capture: :all_but_first)
    |> List.flatten()
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&HTML.strip_tags/1)
    |> Enum.map(&%{id: &1, anchor: URI.encode(HTML.text_to_id(&1))})
  end

  def module_summary(module_node) do
    entries =
      docs_groups(module_node.docs_groups, module_node.docs ++ module_node.typespecs)

    Enum.reject(entries, fn {_type, nodes} -> nodes == [] end)
  end

  defp docs_groups(groups, docs) do
    for group <- groups, do: {group, Enum.filter(docs, &(&1.group == group))}
  end

  defp logo_path(%{logo: nil}), do: nil
  defp logo_path(%{logo: logo}), do: "assets/logo#{Path.extname(logo)}"

  defp sidebar_type(:exception), do: "modules"
  defp sidebar_type(:module), do: "modules"
  defp sidebar_type(:behaviour), do: "modules"
  defp sidebar_type(:protocol), do: "modules"
  defp sidebar_type(:task), do: "tasks"

  defp sidebar_type(:search), do: "search"
  defp sidebar_type(:cheatmd), do: "extras"
  defp sidebar_type(:livemd), do: "extras"
  defp sidebar_type(:extra), do: "extras"

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

  # TODO: Move link_headings and friends to html.ex or even to autolinking code,
  # so content is built with it upfront instead of added at the template level.

  @doc """
  Link headings found with `regex` with in the given `content`. IDs are
  prefixed with `prefix`.
  """
  @heading_regex ~r/<(h[23]).*?>(.*?)<\/\1>/m
  @spec link_headings(String.t() | nil, Regex.t(), String.t()) :: String.t() | nil
  def link_headings(content, regex \\ @heading_regex, prefix \\ "")
  def link_headings(nil, _, _), do: nil

  def link_headings(content, regex, prefix) do
    regex
    |> Regex.scan(content)
    |> Enum.reduce({content, %{}}, fn [match, tag, title], {content, occurrences} ->
      possible_id = HTML.text_to_id(title)
      id_occurred = Map.get(occurrences, possible_id, 0)

      anchor_id = if id_occurred >= 1, do: "#{possible_id}-#{id_occurred}", else: possible_id
      replacement = link_heading(match, tag, title, anchor_id, prefix)
      linked_content = String.replace(content, match, replacement, global: false)
      incremented_occs = Map.put(occurrences, possible_id, id_occurred + 1)
      {linked_content, incremented_occs}
    end)
    |> elem(0)
  end

  @class_regex ~r/<h[23].*?(\sclass="(?<class>[^"]+)")?.*?>/
  @class_separator " "
  defp link_heading(match, _tag, _title, "", _prefix), do: match

  defp link_heading(match, tag, title, id, prefix) do
    section_header_class_name = "section-heading"

    # NOTE: This addition is mainly to preserve the previous `class` attributes
    # from the headers, in case there is one. Now with the _admonition_ text
    # block, we inject CSS classes. So far, the supported classes are:
    # `warning`, `info`, `error`, and `neutral`.
    #
    # The Markdown syntax that we support for the admonition text
    # blocks is something like this:
    #
    #     > ### Never open this door! {: .warning}
    #     >
    #     > ...
    #
    # That should produce the following HTML:
    #
    #      <blockquote>
    #        <h3 class="warning">Never open this door!</h3>
    #        <p>...</p>
    #      </blockquote>
    #
    # The original implementation discarded the previous CSS classes. Instead,
    # it was setting `#{section_header_class_name}` as the only CSS class
    # associated with the given header.
    class_attribute =
      case Regex.named_captures(@class_regex, match) do
        %{"class" => ""} ->
          section_header_class_name

        %{"class" => previous_classes} ->
          # Let's make sure that the `section_header_class_name` is not already
          # included in the previous classes for the header
          previous_classes
          |> String.split(@class_separator)
          |> Enum.reject(&(&1 == section_header_class_name))
          |> Enum.join(@class_separator)
          |> Kernel.<>(" #{section_header_class_name}")
      end

    """
    <#{tag} id="#{prefix}#{id}" class="#{class_attribute}">
      <a href="##{prefix}#{id}" class="hover-link">
        <i class="ri-link-m" aria-hidden="true"></i>
      </a>
      <span class="text">#{title}</span>
    </#{tag}>
    """
  end

  def link_moduledoc_headings(content) do
    link_headings(content, @heading_regex, "module-")
  end

  def link_detail_headings(content, prefix) do
    link_headings(content, @heading_regex, prefix <> "-")
  end

  templates = [
    detail_template: [:node, :module],
    footer_template: [:config, :node],
    head_template: [:config, :page],
    module_template: [:config, :module, :summary, :nodes_map],
    not_found_template: [:config, :nodes_map],
    api_reference_entry_template: [:module_node],
    api_reference_template: [:nodes_map],
    extra_template: [:config, :node, :type, :nodes_map, :refs],
    search_template: [:config, :nodes_map],
    sidebar_template: [:config, :nodes_map],
    summary_template: [:name, :nodes],
    redirect_template: [:config, :redirect_to]
  ]

  Enum.each(templates, fn {name, args} ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    @doc false
    EEx.function_from_file(:def, name, filename, args, trim: true)
  end)
end
