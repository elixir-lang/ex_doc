defmodule ExDoc do
  defmodule Config do
    defstruct [
      formatter: "html",
      formatter_opts: [],
      homepage_url: nil,
      main: nil,
      output: "doc",
      project: nil,
      readme: nil,
      retriever: ExDoc.Retriever,
      source_beam: nil,
      source_root: nil,
      source_url: nil,
      source_url_pattern: nil,
      version: nil,
    ]
  end

  @doc """
  Returns the path to the "templates" dir
  """
  @spec template_dir() :: binary
  def template_dir() do
    Path.expand("../templates", __DIR__)
  end

  @doc """
  Generates documentation for the given `project`, `version`
  and `options`.
  """
  @spec generate_docs(String.t, String.t, Keyword.t) :: atom
  def generate_docs(project, version, options) when is_binary(project) and is_binary(version) and is_list(options) do
    config = build_config(project, version, options)
    docs = config.retriever.docs_from_dir(config.source_beam, config)
    find_formatter(config.formatter).run(docs, config)
  end

  # Builds configuration by merging `options`, and normalizing the options.
  @spec build_config(String.t, String.t, Keyword.t) :: %ExDoc.Config{}
  defp build_config(project, version, options) do
    options = normalize_options(options)
    preconfig = %Config{
      project: project,
      version: version,
      main: options[:main],
      homepage_url: options[:homepage_url],
      source_root: options[:source_root] || File.cwd!,
    }
    struct(preconfig, options)
  end

  # Short path for programmatic interface
  defp find_formatter(modname) when is_atom(modname), do: modname

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
    options = Keyword.put(options, :source_url_pattern, pattern)

    if is_bitstring(options[:output]) do
      options = Keyword.put(options, :output, String.rstrip(options[:output], ?/))
    end

    options
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
