defmodule ExDoc.Formatter.HTML.SearchContents do
  @moduledoc """
  Generates a JSON in order to create an index for lunr.js with
  searchable content.
  """

  alias ExDoc.Formatter.HTML
  alias ExDoc.Formatter.HTML.Templates
  alias ExDoc.Markdown

  def create_search_items(nodes_map, extras) do
    nodes_map =
      nodes_map
      |> Enum.map(&search_items_keys/1)
      |> List.flatten()

    nodes_map = List.flatten([nodes_map | search_items_extras(extras)])

    "searchNodes=[#{Enum.join(nodes_map, ",")}]"
  end

  defp search_items_extras(extras) do
    extras
    |> Enum.map(&search_items_extra/1)
    |> List.flatten()
  end

  @h2_split_regex ~r/<h2.*?>/
  @header_body_regex ~r/(?<header>.*)<\/h2>(?<body>.*)/s
  defp search_items_extra(%{id: id, title: title, content: content}) do
    [intro | sections] = Regex.split(@h2_split_regex, content)
    intro_json_item = encode_extra(title, id, intro)

    section_json_items =
      sections
      |> Enum.map(&Regex.named_captures(@header_body_regex, &1))
      |> Enum.map(&search_items_extra_section(title, &1["header"], &1["body"], id))

    [intro_json_item | section_json_items]
  end

  defp search_items_extra_section(title, header, body, id) do
    header = HTML.strip_tags(header)
    encode_extra(title, id, body, header)
  end

  defp search_items_keys({_type, value}) do
    value
    |> Enum.map(&search_items_node/1)
  end

  defp search_items_node(node = %ExDoc.ModuleNode{}) do
    modules = encode_node(node.id, node.id, node.type, node.doc)
    functions = Enum.map(node.docs, &search_items_node(&1, node))
    types = Enum.map(node.typespecs, &search_items_node(&1, node))
    [modules | [functions | types]]
  end

  defp search_items_node(node, parent) do
    encode_node(parent.id, node.id, node.type, node.doc)
  end

  defp encode_extra(title, id, doc) do
    encode("#{id}.html", title, nil, id, :extras, doc)
  end

  defp encode_extra(title, id, doc, header) do
    encode(
      "#{id}.html##{HTML.text_to_id(header)}",
      "#{title} - #{header}",
      nil,
      id,
      :extras,
      doc
    )
  end

  defp encode_node(module, id, type, nil), do: encode_node(module, id, type, "")

  defp encode_node(module, id, type, doc) do
    {ref, title, text} =
      case type do
        type when type in [:module, :behaviour, :exception, :protocol, :task] ->
          {
            "#{module}.html",
            id,
            String.replace(id, ".", " ")
          }

        _ ->
          {
            "#{module}.html##{Templates.link_id(id, type)}",
            "#{module}.#{id}",
            String.replace(module, ".", " ") <> " #{split_title(id)}"
          }
      end

    encode(
      ref,
      title,
      text,
      module,
      type,
      Markdown.to_html(doc)
    )
  end

  defp split_title(title) do
    match = Regex.named_captures(~r/(?<name>\w+[\?|!]?)\/(?<arity>\d)/, title)
    title = "#{match["name"]}/#{match["arity"]} #{match["name"]}"
    name = match["name"]

    if name && name =~ "_" do
      "#{title} " <> String.replace(name, "_", " ")
    else
      title
    end
  end

  defp encode(ref, title, text, module, type, doc) do
    Poison.encode!(%{
      ref: ref,
      title: title,
      text: text,
      module: module,
      type: type,
      doc: clean_doc(doc)
    })
  end

  defp clean_doc(nil), do: clean_doc("")

  defp clean_doc(doc) do
    doc
    |> HTML.strip_tags()
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
  end
end
