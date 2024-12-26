defmodule ExDoc.Formatter.MARKDOWN.Templates do
  @moduledoc false

  require EEx

  import ExDoc.Utils,
    only: [before_closing_body_tag: 2, h: 1, text_to_id: 1]

  alias ExDoc.Formatter.HTML.Templates, as: H

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(config, module_node) do
    summary = H.module_summary(module_node)
    module_template(config, module_node, summary)
  end

  @doc """
  Generated ID for static file
  """
  def static_file_to_id(static_file) do
    static_file |> Path.basename() |> text_to_id()
  end

  def node_doc(%{source_doc: %{"en"=> source}}), do: source
  def node_doc(%{rendered_doc: source}), do: source

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
        [left, _] -> String.trim_trailing(left, ": ")
        [all] -> all
      end

    # Remove any anchors found in synopsis.
    # Old Erlang docs placed anchors at the top of the documentation
    # for links. Ideally they would have been removed but meanwhile
    # it is simpler to guarantee they won't be duplicated in docs.
    Regex.replace(~r|(<[^>]*) id="[^"]*"([^>]*>)|, doc, ~S"\1\2", [])
  end
  
    @doc """
  Add link headings for the given `content`.

  IDs are prefixed with `prefix`.

  We only link `h2` and `h3` headers. This is kept consistent in ExDoc.SearchData.
  """
  @heading_regex ~r/<(h[23]).*?>(.*?)<\/\1>/m
  @spec link_headings(String.t() | nil, String.t()) :: String.t() | nil
  def link_headings(content, prefix \\ "")
  def link_headings(nil, _), do: nil

  def link_headings(content, prefix) do
    @heading_regex
    |> Regex.scan(content)
    |> Enum.reduce({content, %{}}, fn [match, tag, title], {content, occurrences} ->
      possible_id = text_to_id(title)
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

    # The Markdown syntax that we support for the admonition text
    # blocks is something like this:
    #
    #     > ### Never open this door! {: .warning}
    #     >
    #     > ...
    #

    """
    ## [#{title}](##{prefix}#{id})
    """
  end

  def link_moduledoc_headings(content) do
    link_headings(content, "module-")
  end

  def link_detail_headings(content, prefix) do
    link_headings(content, prefix <> "-")
  end

  @doc """
  Creates a chapter which contains all the details about an individual module.

  This chapter can include the following sections: *functions*, *types*, *callbacks*.
  """
  EEx.function_from_file(
    :def,
    :module_template,
    Path.expand("templates/module_template.eex", __DIR__),
    [:config, :module, :summary],
    trim: true
  )

  @doc """
  Creates the table of contents.

  """
  EEx.function_from_file(
    :def,
    :nav_template,
    Path.expand("templates/nav_template.eex", __DIR__),
    [:config, :nodes],
    trim: true
  )


  EEx.function_from_file(
    :defp,
    :nav_item_template,
    Path.expand("templates/nav_item_template.eex", __DIR__),
    [:name, :nodes],
    trim: true
  )

  EEx.function_from_file(
    :defp,
    :nav_grouped_item_template,
    Path.expand("templates/nav_grouped_item_template.eex", __DIR__),
    [:nodes],
    trim: true
  )

  # EEx.function_from_file(
  #   :defp,
  #   :toc_item_template,
  #   Path.expand("templates/toc_item_template.eex", __DIR__),
  #   [:nodes],
  #   trim: true
  # )


  # def media_type(_arg), do: nil

  templates = [
    detail_template: [:node, :module],
    summary_template: [:name, :nodes]
  ]

  Enum.each(templates, fn {name, args} ->
    filename = Path.expand("templates/#{name}.eex", __DIR__)
    @doc false
    EEx.function_from_file(:def, name, filename, args, trim: true)
  end)
end
