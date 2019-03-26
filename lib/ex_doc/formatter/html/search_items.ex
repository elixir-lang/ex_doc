defmodule ExDoc.Formatter.HTML.SearchItems do
  @moduledoc false

  alias ExDoc.Formatter.HTML
  alias ExDoc.Formatter.HTML.Templates
  alias ExDoc.Markdown

  def create_search_items(nodes_map, extras) do
    items = search_items_keys(nodes_map) ++ search_items_extras(extras)
    ["searchNodes=[", Enum.intersperse(items, ?,), "]"]
  end

  defp search_items_extras(extras) do
    Enum.flat_map(extras, &search_items_extra/1)
  end

  @h2_split_regex ~r/<h2.*?>/
  @header_body_regex ~r/(?<header>.*)<\/h2>(?<body>.*)/s
  defp search_items_extra(%{id: id, title: title, content: content}) do
    [intro | sections] = Regex.split(@h2_split_regex, content)
    intro_json_item = encode("#{id}.html", title, id, :extras, intro)

    section_json_items =
      sections
      |> Enum.map(&Regex.named_captures(@header_body_regex, &1))
      |> Enum.map(&search_items_extra_section(title, &1["header"], &1["body"], id))

    [intro_json_item | section_json_items]
  end

  defp search_items_extra_section(title, header, body, id) do
    header = HTML.strip_tags(header)

    encode(
      "#{id}.html##{HTML.text_to_id(header)}",
      "#{title} - #{header}",
      id,
      :extras,
      body
    )
  end

  defp search_items_keys(nodes_map) do
    for {_type, nodes} <- nodes_map,
        node <- nodes,
        entry <- search_items_node(node),
        do: entry
  end

  defp search_items_node(node = %ExDoc.ModuleNode{id: id, type: type, doc: doc}) do
    module = encode("#{id}.html", id, id, type, Markdown.to_html(doc || ""))
    functions = Enum.map(node.docs, &search_items_node_child(&1, id))
    types = Enum.map(node.typespecs, &search_items_node_child(&1, id))
    [module] ++ functions ++ types
  end

  defp search_items_node_child(%{id: id, type: type, doc: doc}, module) do
    encode(
      "#{module}.html##{Templates.link_id(id, type)}",
      "#{module}.#{id}",
      module,
      type,
      Markdown.to_html(doc || "")
    )
  end

  defp encode(ref, title, module, type, doc) do
    [
      "{\"ref\":",
      [?", ref, ?"],
      ",\"title\":",
      [?", title, ?"],
      ",\"module\":",
      [?", module, ?"],
      ",\"type\":",
      [?", Atom.to_string(type), ?"],
      ",\"doc\":",
      clean_doc(doc),
      "}"
    ]
  end

  defp clean_doc(doc) do
    doc
    |> Kernel.||("")
    |> HTML.strip_tags()
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
    |> inspect(printable_limit: :infinity)
  end
end
