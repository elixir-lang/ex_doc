defmodule ExDoc.Formatter.HTML.SearchItems do
  @moduledoc false

  # TODO: It should not depend on the parent module
  alias ExDoc.Formatter.HTML

  def create(nodes, extras) do
    items = Enum.flat_map(nodes, &module/1) ++ Enum.flat_map(extras, &extra/1)
    ["searchNodes=" | ExDoc.Utils.to_json(items)]
  end

  defp extra(map) do
    [intro | sections] = Regex.split(~r/## (?<header>\b.+)/, map.source, include_captures: true)

    sections = Enum.chunk_every(sections, 2)

    intro_json_item =
      encode(
        "#{map.id}.html",
        map.title,
        :extras,
        clean_markdown(intro)
      )

    section_json_items =
      for [header, body] <- sections do
        "## " <> header = header

        encode(
          "#{map.id}.html##{HTML.text_to_id(header)}",
          "#{map.title} - " <> clean_markdown(header),
          :extras,
          clean_markdown(body)
        )
      end

    [intro_json_item | section_json_items]
  end

  @doc """
  TODO
  """
  def module(%ExDoc.ModuleNode{} = node) do
    module =
      encode(
        "#{node.id}.html",
        node.id,
        node.type,
        search_doc(node.doc_format, node)
      )

    functions = Enum.map(node.docs, &node_child(&1, node))
    types = Enum.map(node.typespecs, &node_child(&1, node))
    [module] ++ functions ++ types
  end

  defp node_child(node, module_node) do
    encode(
      "#{module_node.id}.html##{node.id}",
      "#{module_node.id}.#{node.name}/#{node.arity}",
      node.type,
      search_doc(module_node.doc_format, node)
    )
  end

  defp encode(ref, title, type, doc) do
    %{
      ref: URI.encode(ref),
      title: title,
      type: type,
      doc: doc
    }
  end

  defp search_doc("text/markdown", %{source_doc: %{"en" => doc}}) do
    clean_markdown(doc)
  end

  defp search_doc("application/erlang+html", %{rendered_doc: doc}) do
    doc
    |> HTML.strip_tags(" ")
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
  end

  defp search_doc(_format, _node) do
    ""
  end

  defp clean_markdown(doc) do
    doc
    |> HTML.strip_tags(" ")
    |> String.trim()
  end
end
