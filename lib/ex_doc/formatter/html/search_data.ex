defmodule ExDoc.Formatter.HTML.SearchData do
  @moduledoc false
  alias ExDoc.Utils

  def create(nodes, extras, proglang) do
    items = Enum.flat_map(nodes, &module/1) ++ Enum.flat_map(extras, &extra/1)

    data = %{
      items: items,
      content_type: "text/markdown",
      proglang: proglang,
      producer: %{
        name: "ex_doc",
        version: to_string(Application.spec(:ex_doc)[:vsn])
      }
    }

    ["searchData=" | ExDoc.Utils.to_json(data)]
  end

  defp extra(map) do
    if custom_search_data = map[:search_data] do
      extra_search_data(map, custom_search_data)
    else
      {intro, sections} = extract_sections_from_markdown(map.source)

      intro_json_item =
        encode(
          "#{map.id}.html",
          map.title,
          :extras,
          intro
        )

      section_json_items =
        for {header, body} <- sections do
          encode(
            "#{map.id}.html##{Utils.text_to_id(header)}",
            header <> " - #{map.title}",
            :extras,
            body
          )
        end

      [intro_json_item | section_json_items]
    end
  end

  defp extra_search_data(map, custom_search_data) do
    Enum.map(custom_search_data, fn item ->
      link =
        if item.anchor === "" do
          "#{map.id}.html"
        else
          "#{map.id}.html##{item.anchor}"
        end

      encode(link, item.title <> " - #{map.id}", item.type, clean_markdown(item.body))
    end)
  end

  defp module(%ExDoc.ModuleNode{} = node) do
    {intro, sections} = extract_sections(node.doc_format, node)

    module =
      encode(
        "#{node.id}.html",
        node.title,
        node.type,
        intro
      )

    module_sections =
      for {header, body} <- sections do
        encode(
          "#{node.id}.html#module-#{Utils.text_to_id(header)}",
          header <> " - #{node.title}",
          node.type,
          body
        )
      end

    docs = Enum.flat_map(node.docs, &node_child(&1, node))
    [module] ++ module_sections ++ docs
  end

  defp node_child(node, module_node) do
    {intro, sections} = extract_sections(module_node.doc_format, node)

    child =
      encode(
        "#{module_node.id}.html##{node.id}",
        "#{module_node.id}.#{node.name}/#{node.arity}",
        node.type,
        intro
      )

    child_sections =
      for {header, body} <- sections do
        encode(
          "#{module_node.id}.html##{node.id}-#{Utils.text_to_id(header)}",
          header <> " - #{module_node.id}.#{node.name}/#{node.arity}",
          node.type,
          body
        )
      end

    [child] ++ child_sections
  end

  defp encode(ref, title, type, doc) do
    %{
      ref: URI.encode(ref),
      title: title,
      type: type,
      doc: doc
    }
  end

  defp extract_sections("text/markdown", %{source_doc: %{"en" => doc}}) do
    extract_sections_from_markdown(doc)
  end

  defp extract_sections("application/erlang+html", %{rendered_doc: nil}) do
    {nil, []}
  end

  defp extract_sections("application/erlang+html", %{rendered_doc: doc}) do
    {clean_html(doc), []}
  end

  defp extract_sections(_format, _doc) do
    {"", []}
  end

  defp extract_sections_from_markdown(string) do
    [intro | sections] =
      Regex.split(~r/(?<!#)###? (?<header>\b.+)/, string, include_captures: true)

    sections =
      for [header, section] <- Enum.chunk_every(sections, 2) do
        header = String.trim_leading(header, "#")

        section =
          section
          |> ExDoc.Utils.strip_tags(" ")
          |> drop_ignorable_codeblocks()
          |> String.trim()

        {clean_markdown(header), section}
      end

    {clean_markdown(intro), sections}
  end

  defp clean_markdown(doc) do
    doc
    |> ExDoc.Utils.strip_tags(" ")
    |> String.trim()
  end

  defp clean_html(doc) do
    doc
    |> ExDoc.Utils.strip_tags(" ")
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
  end

  @ignored_codeblocks ~w[vega-lite]

  defp drop_ignorable_codeblocks(section) do
    block_names = Enum.join(@ignored_codeblocks, "|")
    String.replace(section, ~r/^```(?:#{block_names})\n(?:[\s\S]*?)```$/m, "")
  end
end
