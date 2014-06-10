defmodule ExDoc do
  defmodule Config do
    defstruct [
      output: "docs", source_root: nil, source_url: nil, source_url_pattern: nil,
      homepage_url: nil, source_beam: nil, retriever: ExDoc.Retriever,
      formatter: "html", project: nil, version: nil, main: nil,
      readme: false
    ]
  end

  @markdown_processors [ExDoc.Markdown.Sundown, ExDoc.Markdown.Pandoc]
  @markdown_processor_key :markdown_processor

  @doc """
  Generates documentation for the given `project`, `version`
  and `options`.
  """
  def generate_docs(project, version, options) when is_binary(project) and is_binary(version) and is_list(options) do
    init_markdown_processor()

    options = normalize_options(options)
    config  = %Config{project: project, version: version, main: options[:main] || project,
                      homepage_url: options[:homepage_url],
                      source_root: options[:source_root] || File.cwd!}
    config = struct(config, options)

    docs = config.retriever.docs_from_dir(config.source_beam, config)
    find_formatter(config.formatter).run(docs, config)
  end

  @doc false
  # This is made public for use in tests
  def init_markdown_processor(candidates \\ @markdown_processors) do
    {processor, errors, _} = Enum.reduce(candidates, {nil, [], :continue}, fn
      _, {module, _, :stop} -> {module, nil, :stop}

      module, {_, errors, _} ->
        case module.init() do
          :ok -> {module, nil, :stop}
          {:error, reason} -> {nil, [{module, reason}|errors], :continue}
        end
    end)
    unless processor do
      IO.puts "Failed to initialize markdown processor:"
      errors |> Enum.reverse |> Enum.each(fn {module, reason} ->
        IO.puts "  #{inspect module}: #{inspect reason}"
      end)
      System.halt(1)
    end
  end

  @doc false
  # This function is used by individual processors
  def register_markdown_processor(module) do
    :application.set_env(:ex_doc, @markdown_processor_key, module)
  end

  @doc false
  # This is used by the ExDoc.Markdown
  def get_markdown_processor() do
    case :application.get_env(:ex_doc, @markdown_processor_key) do
      {:ok, module} -> module
      :undefined -> false
    end
  end

  # short path for programmatic interface
  defp find_formatter(modname) when is_atom(modname), do: modname

  # short path for the stock formatters
  defp find_formatter("html"), do: ExDoc.Formatter.HTML

  defp find_formatter("ExDoc.Formatter." <> _ = name) do
    Module.concat([name]) |> check_formatter_module(name)
  end

  defp find_formatter(name) do
    Module.concat([ExDoc.Formatter, String.upcase(name)])
    |> check_formatter_module(name)
  end

  defp check_formatter_module(modname, argname) do
    unless Code.ensure_loaded?(modname) do
      raise "Formatter module not found for: #{argname}"
    end
    modname
  end

  # Helpers

  defp normalize_options(options) do
    pattern = options[:source_url_pattern] || guess_url(options[:source_url], options[:source_ref] || "master")
    Keyword.put(options, :source_url_pattern, pattern)
  end

  defp guess_url(url = <<"https://github.com/", _ :: binary>>, ref) do
    append_slash(url) <> "blob/#{ref}/%{path}#L%{line}"
  end

  defp guess_url(url = <<"https://bitbucket.org/", _ :: binary>>, ref) do
    append_slash(url) <> "src/#{ref}/%{path}#cl-%{line}"
  end

  defp guess_url(other, _) do
    other
  end

  defp append_slash(url) do
    if :binary.last(url) == ?/, do: url, else: url <> "/"
  end
end
