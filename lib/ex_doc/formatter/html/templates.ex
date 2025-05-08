defmodule ExDoc.Formatter.HTML.Templates do
  @moduledoc false
  require EEx
  alias ExDoc.Formatter.HTML.Assets

  import ExDoc.Utils,
    only: [
      h: 1,
      before_closing_body_tag: 2,
      before_closing_footer_tag: 2,
      before_closing_head_tag: 2,
      text_to_id: 1
    ]

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(module_node, config) do
    summary = module_summary(module_node)
    module_template(config, module_node, summary)
  end

  @doc """
  Format the attribute type used to define the spec of the given `node`.
  """
  def format_spec_attribute(module, node) do
    module.language.format_spec_attribute(node)
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
      %{id: id, title: title, group: group} = extra
      item = %{id: to_string(id), title: to_string(title), group: to_string(group)}

      case extra do
        %{search_data: search_data} when is_list(search_data) ->
          search_data =
            Enum.map(search_data, fn search_item ->
              %{
                anchor: search_item.anchor,
                id: search_item.title,
                labels: [search_item.type]
              }
            end)

          item
          |> Map.put(:headers, headers_to_id_and_anchors(extra.headers))
          |> Map.put(:searchData, search_data)

        %{url: url} when is_binary(url) ->
          Map.put(item, :url, url)

        _ ->
          Map.put(item, :headers, headers_to_id_and_anchors(extra.headers))
      end
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

    %{key: text_to_id(group), name: group, nodes: nodes}
  end

  defp module_sections(module) do
    sections =
      (module.doc || [])
      |> ExDoc.DocAST.extract_headers_with_ids([:h2])
      |> headers_to_id_and_anchors()

    [sections: sections]
  end

  defp headers_to_id_and_anchors(headers) do
    Enum.map(headers, fn {:h2, text, anchor} ->
      %{id: text, anchor: anchor}
    end)
  end

  def module_summary(module_node) do
    # TODO: Maybe it should be moved to retriever and it already returned grouped metadata
    ExDoc.GroupMatcher.group_by(module_node.docs_groups, module_node.docs, & &1.group)
  end

  defp favicon_path(%{favicon: nil}), do: nil
  defp favicon_path(%{favicon: favicon}), do: "assets/favicon#{Path.extname(favicon)}"

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

  # TODO: Adding the link headings must be done via DocAST instead of using regexes.

  @doc """
  Add link headings for the given `content`.

  IDs are prefixed with `prefix`.

  We only link `h2` and `h3` headers. This is kept consistent in ExDoc.SearchData.
  """
  @spec link_headings(String.t() | nil, String.t()) :: String.t() | nil
  def link_headings(content, prefix \\ "")
  def link_headings(nil, _), do: nil

  def link_headings(content, prefix) do
    ~r/<(h[23]).*?>(.*?)<\/\1>/m
    |> Regex.scan(content)
    |> Enum.reduce({content, %{}}, fn [match, tag, title], {content, occurrences} ->
      possible_id = title |> ExDoc.Utils.strip_tags() |> text_to_id()
      id_occurred = Map.get(occurrences, possible_id, 0)

      anchor_id = if id_occurred >= 1, do: "#{possible_id}-#{id_occurred}", else: possible_id
      replacement = link_heading(match, tag, title, anchor_id, prefix)
      linked_content = String.replace(content, match, replacement, global: false)
      incremented_occs = Map.put(occurrences, possible_id, id_occurred + 1)
      {linked_content, incremented_occs}
    end)
    |> elem(0)
  end

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
      case Regex.named_captures(~r/<h[23].*?(\sclass="(?<class>[^"]+)")?.*?>/, match) do
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
    link_headings(content, "module-")
  end

  def link_detail_headings(content, prefix) do
    link_headings(content, prefix <> "-")
  end

  templates = [
    detail_template: [:node, :module],
    footer_template: [:config, :node],
    head_template: [:config, :title, :noindex],
    module_template: [:config, :module, :summary],
    not_found_template: [:config],
    api_reference_entry_template: [:module_node],
    api_reference_template: [:nodes_map],
    extra_template: [:config, :node, :type, :refs],
    search_template: [:config],
    sidebar_template: [:config, :type],
    summary_template: [:name, :nodes],
    redirect_template: [:config, :redirect_to]
  ]

  Enum.each(templates, fn {name, args} ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    @doc false
    EEx.function_from_file(:def, name, filename, args, trim: true)
  end)
end
