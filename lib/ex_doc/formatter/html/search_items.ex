defmodule ExDoc.Formatter.HTML.SearchItems do
  @moduledoc false
  alias ExDoc.Formatter.HTML

  def create(nodes, extras) do
    items = Enum.flat_map(nodes, &module_node/1) ++ Enum.flat_map(extras, &extra/1)
    ["searchNodes=[", Enum.intersperse(items, ?,), "]"]
  end

  @h2_split_regex ~r/<h2.*?>/
  @header_body_regex ~r/(?<header>.*)<\/h2>(?<body>.*)/s
  defp extra(%{id: id, title: title, content: content}) do
    [intro | sections] = Regex.split(@h2_split_regex, content)
    intro_json_item = encode("#{id}.html", title, :extras, intro)

    section_json_items =
      sections
      |> Enum.map(&Regex.named_captures(@header_body_regex, &1))
      |> Enum.map(&extra_section(title, &1["header"], &1["body"], id))

    [intro_json_item | section_json_items]
  end

  defp extra_section(title, header, body, id) do
    header = HTML.strip_tags(header)

    encode(
      "#{id}.html##{HTML.text_to_id(header)}",
      "#{title} - #{header}",
      :extras,
      body
    )
  end

  defp module_node(node = %ExDoc.ModuleNode{id: id, type: type, rendered_doc: doc}) do
    module = encode("#{id}.html", id, type, doc)
    functions = Enum.map(node.docs, &node_child(&1, id))
    types = Enum.map(node.typespecs, &node_child(&1, id))
    [module] ++ functions ++ types
  end

  defp node_child(%{id: id, type: type, rendered_doc: doc}, module) do
    encode(
      "#{module}.html##{HTML.link_id(id, type)}",
      "#{module}.#{id}",
      type,
      doc
    )
  end

  defp encode(ref, title, type, doc) do
    [
      "{\"ref\":",
      [?", ref, ?"],
      ",\"title\":",
      [?", title, ?"],
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
