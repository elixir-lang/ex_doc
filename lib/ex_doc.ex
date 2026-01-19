defmodule ExDoc do
  @moduledoc false
  @ex_doc_version Mix.Project.config()[:version]

  @doc """
  Returns the ExDoc version (used in templates).
  """
  @spec version :: String.t()
  def version, do: @ex_doc_version

  @doc """
  Emits a warning.
  """
  def warn(message, stacktrace_info) do
    :persistent_term.put({__MODULE__, :warned?}, true)
    IO.warn(message, stacktrace_info)
  end

  defp unset_warned() do
    warned? = :persistent_term.get({__MODULE__, :warned?}, false)
    :persistent_term.erase({__MODULE__, :warned?})
    warned?
  end

  @doc """
  Generates documentation for the given `project`, `vsn` (version)
  and `options`.
  """
  @spec generate(String.t(), String.t(), Keyword.t()) ::
          [%{entrypoint: String.t(), warned?: boolean(), formatter: module()}]
  def generate(project, version, options)
      when is_binary(project) and is_binary(version) and is_list(options) do
    # Clear it up for tests
    _ = unset_warned()

    source_beam = Keyword.get(options, :source_beam)
    retriever = Keyword.get(options, :retriever, ExDoc.Retriever)
    extras_input = Keyword.get(options, :extras, [])

    if processor = options[:markdown_processor] do
      ExDoc.Markdown.put_markdown_processor(processor)
    end

    # Build configs independently (build both upfront for validation)
    retriever_config = ExDoc.Config.build(options)
    formatter_config = ExDoc.Formatter.Config.build(project, version, options)

    # Retriever phase (run once for all formatters)
    {modules, filtered} = retriever.docs_from_dir(source_beam, retriever_config)
    extras = ExDoc.Extras.build(extras_input, retriever_config)

    for formatter <- formatter_config.formatters do
      formatter = find_formatter(formatter)
      entrypoint = ExDoc.Formatter.run(formatter, formatter_config, modules, filtered, extras)

      %{entrypoint: entrypoint, warned?: unset_warned(), formatter: formatter}
    end
  end

  defp find_formatter(modname) when is_atom(modname),
    do: modname

  defp find_formatter("ExDoc.Formatter." <> _ = name),
    do: Module.concat([name])

  defp find_formatter(name),
    do: Module.concat([ExDoc.Formatter, String.upcase(name)])
end
