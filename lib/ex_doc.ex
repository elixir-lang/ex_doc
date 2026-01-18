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

    # Build configs independently
    retriever_config = ExDoc.Config.build(options)
    formatter_config = ExDoc.Formatter.Config.build(project, vsn, options)

    if processor = options[:markdown_processor] do
      ExDoc.Markdown.put_markdown_processor(processor)
    end

    # Retriever phase
    {module_nodes, filtered_nodes} = retriever.docs_from_dir(source_beam, retriever_config)
    extras = ExDoc.Extras.build(extras_input, retriever_config)

    # Update formatter config with runtime data
    formatter_config = %{formatter_config | filtered_modules: filtered_nodes}

    # Formatter phase
    find_formatter(formatter).run(module_nodes, extras, formatter_config)
  end

  # Short path for programmatic interface
  defp find_formatter(modname) when is_atom(modname), do: modname

  defp find_formatter("ExDoc.Formatter." <> _ = name) do
    [name]
    |> Module.concat()
    |> check_formatter_module(name)
  end

  defp find_formatter(name) do
    [ExDoc.Formatter, String.upcase(name)]
    |> Module.concat()
    |> check_formatter_module(name)
  end

  defp check_formatter_module(modname, argname) do
    if Code.ensure_loaded?(modname) do
      modname
    else
      raise "formatter module #{inspect(argname)} not found"
    end
  end
end
