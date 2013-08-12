defmodule ExDoc do
  defrecord Config, output: "docs", source_root: nil, source_url: nil, source_url_pattern: nil,
                    homepage_url: nil, source_beam: nil, formatter: ExDoc.HTMLFormatter,
                    project: nil, version: nil, main: nil, readme: false

  @doc """
  Generates documentation for the given `project`, `version`
  and `options`.
  """
  def generate_docs(project, version, options) when is_binary(project) and is_binary(version) and is_list(options) do
    options = normalize_options(options)
    config  = Config[project: project, version: version, main: options[:main] || project,
                     homepage_url: options[:homepage_url],
                     source_root: options[:source_root] || File.cwd!].update(options)

    source_beam = config.source_beam || Path.join(config.source_root, "ebin")
    docs = ExDoc.Retriever.docs_from_dir(source_beam, config)

    config.formatter.run(docs, config)
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
