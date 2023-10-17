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
    config = ExDoc.Config.build(project, vsn, options)

    if processor = options[:markdown_processor] do
      ExDoc.Markdown.put_markdown_processor(processor)
    end

    {module_nodes, filtered_nodes} = config.retriever.docs_from_dir(config.source_beam, config)
    find_formatter(config.formatter).run(module_nodes, filtered_nodes, config)
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
