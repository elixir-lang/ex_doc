defmodule ExDoc do
  @moduledoc false

  alias ExDoc.Config

  @doc "Returns the ExDoc version. (Used in templates.)"
  @spec version :: String.t

  @ex_doc_version Mix.Project.config()[:version]
  def version, do: @ex_doc_version

  @doc "Generates docs for the given `project`, `vsn` (version) & `options`."
  @spec generate_docs(String.t, String.t, Keyword.t) :: atom
  def generate_docs(project, version, options) when is_binary(project)
                                               and  is_binary(version)
                                               and  is_list(options)
  do
    options = normalize_options(options)
    config  = struct(%Config{
                       project:     project,
                       version:     version,
                       source_root: Keyword.get(options, :source_root, File.cwd!())
                     }, options)

    # two side-effects
    ExDoc.Markdown.put_markdown_processor(options[:markdown_processor])
    ExDoc.Markdown.configure_processor(options[:markdown_processor_options])

    docs = config.retriever.docs_from_dir(config.source_beam, config)
    find_formatter(config.formatter).run(docs, config) # below `normalize_module_nesting_prefixes/1`
  end

  defp normalize_options(options) do
    pattern = options[:source_url_pattern] ||
              guess_url(options[:source_url], options[:source_ref] ||
                                              ExDoc.Config.default_source_ref())

    Keyword.put(options, :source_url_pattern, pattern)
    |> normalize_output() # below `append_slash/1`
  end

  defp guess_url(url, ref) do
    with {:ok, host_with_path} <- http_or_https(url),
         {:ok, pattern}        <- known_pattern(host_with_path, ref)
    do
      "https://" <> append_slash(host_with_path) <> pattern
    else
      _ -> url
    end
  end

  defp http_or_https("http://"  <> rest), do: {:ok, rest}
  defp http_or_https("https://" <> rest), do: {:ok, rest}
  defp http_or_https(_),                  do: :error

  defp known_pattern("github.com/"    <> _, ref), do: {:ok, "blob/#{ref}/%{path}#L%{line}"}
  defp known_pattern("gitlab.com/"    <> _, ref), do: {:ok, "blob/#{ref}/%{path}#L%{line}"}
  defp known_pattern("bitbucket.org/" <> _, ref), do: {:ok, "src/#{ref}/%{path}#cl-%{line}"}
  defp known_pattern(_host_with_path, _ref),      do: :error

  defp append_slash(url), do:
    if :binary.last(url) == ?/, do: url, else: url <> "/"

  defp normalize_output(options) do
    output = options[:output]

    if is_binary(output) do
      Keyword.put(options, :output, String.trim_trailing(output, "/"))
    else
      options
    end
    |> normalize_module_nesting_prefixes()
  end

  # Sorts `:nest_modules_by_prefix` in descending order. Helps to find longest match.
  defp normalize_module_nesting_prefixes(options) do
    normalized_prefixes = options
                          |> Keyword.get(:nest_modules_by_prefix, [])
                          |> Enum.map(&inspect/1)
                          |> Enum.sort
                          |> Enum.reverse()

    Keyword.put(options, :nest_modules_by_prefix, normalized_prefixes)
  end

  # Short path for programmatic interface
  defp find_formatter(modname) when is_atom(modname),
    do: modname
  defp find_formatter("ExDoc.Formatter." <> _ = name),
    do: [name]
        |> Module.concat()
        |> check_formatter_module(name)
  defp find_formatter(name),
    do: [ExDoc.Formatter, String.upcase(name)]
        |> Module.concat()
        |> check_formatter_module(name)

  defp check_formatter_module(modname, argname), do:
    if Code.ensure_loaded?(modname),
      do:   modname,
      else: raise "formatter module #{inspect(argname)} not found"
end
