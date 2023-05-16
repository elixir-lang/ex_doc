defmodule ExDoc.Formatter.HTML.SearchItems do
  @moduledoc false

  # TODO: It should not depend on the parent module
  alias ExDoc.Formatter.HTML

  def create(nodes, extras) do
    items = Enum.flat_map(nodes, &module/1) ++ Enum.flat_map(extras, &extra/1)
    ["searchNodes=" | ExDoc.Utils.to_json(items)]
  end

  defp extra(map) do
    [intro | sections] =
      Regex.split(~r/## (?<header>\b.+)/, map.source, include_captures: true)

    sections = Enum.chunk_every(sections, 2)

    intro_json_item =
      encode(
        "#{map.id}.html",
        map.title,
        :extras,
        String.trim(intro)
      )

    section_json_items =
      for [header, body] <- sections do
        "## " <> header = header

        encode(
          "#{map.id}.html##{HTML.text_to_id(header)}",
          "#{map.title} - " <> clean_doc(header),
          :extras,
          body
        )
      end

    [intro_json_item | section_json_items]
  end

  defp module(%ExDoc.ModuleNode{} = node) do
    source_doc =
      node.doc_format == "text/markdown" && is_map(node.source_doc) && node.source_doc["en"]

    module =
      encode(
        "#{node.id}.html",
        node.id,
        node.type,
        source_doc || node.rendered_doc
      )

    functions = Enum.map(node.docs, &node_child(&1, node))
    types = Enum.map(node.typespecs, &node_child(&1, node))
    [module] ++ functions ++ types
  end

  defp node_child(node, module_node) do
    source_doc =
      module_node.doc_format == "text/markdown" && is_map(node.source_doc) &&
        node.source_doc["en"]

    encode(
      "#{module_node.id}.html##{node.id}",
      "#{module_node.id}.#{node.name}/#{node.arity}",
      node.type,
      source_doc || node.rendered_doc
    )
  end

  defp encode(ref, title, type, doc) do
    %{
      ref: URI.encode(ref),
      title: title,
      type: type,
      doc: clean_doc(doc)
    }
  end

  defp clean_doc(doc) do
    doc
    |> Kernel.||("")
    |> HTML.strip_tags(" ")
    |> String.replace(~r/[ \t]+/, " ")
    |> String.trim()
  end
end
