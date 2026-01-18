defmodule ExDoc do
  @moduledoc false
  @ex_doc_version Mix.Project.config()[:version]

  @doc """
  Returns the ExDoc version (used in templates).
  """
  @spec version :: String.t()
  def version, do: @ex_doc_version

  @doc """
  Generates documentation for the given `project`, `vsn` (version)
  and `options`.
  """
  @spec generate_docs(String.t(), String.t(), Keyword.t()) :: atom
  def generate_docs(project, vsn, options)
      when is_binary(project) and is_binary(vsn) and is_list(options) do
    formatter = Keyword.get(options, :formatter, "html")
    source_beam = Keyword.get(options, :source_beam)
    retriever = Keyword.get(options, :retriever, ExDoc.Retriever)
    extras_input = Keyword.get(options, :extras, [])

    if processor = options[:markdown_processor] do
      ExDoc.Markdown.put_markdown_processor(processor)
    end

    # Build configs independently (build both upfront for validation)
    retriever_config = ExDoc.Config.build(options)
    formatter_config = ExDoc.Formatter.Config.build(project, vsn, options)

    # Retriever phase
    {module_nodes, filtered_nodes} = retriever.docs_from_dir(source_beam, retriever_config)
    extras = ExDoc.Extras.build(extras_input, retriever_config)

    # Formatter phase
    formatter = find_formatter(formatter)
    ExDoc.Formatter.run(formatter, formatter_config, module_nodes, filtered_nodes, extras)
  end

  defp find_formatter(modname) when is_atom(modname),
    do: modname

  defp find_formatter("ExDoc.Formatter." <> _ = name),
    do: Module.concat([name])

  defp find_formatter(name),
    do: Module.concat([ExDoc.Formatter, String.upcase(name)])
end
