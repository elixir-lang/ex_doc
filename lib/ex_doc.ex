defmodule ExDoc do
  defrecord Config, output: "docs", source_root: nil, source_url: nil, source_url_pattern: nil,
                    source_beam: nil, formatter: ExDoc.HTMLFormatter, project: nil, version: nil,
                    main: nil

  @doc """
  Generates documentation for the given project, version
  and options.
  """
  def generate_docs(project, version, options) when is_binary(project) and is_binary(version) and is_list(options) do
    options = normalize_options(options)
    config  = Config[project: project, version: version, main: options[:main] || project,
                     source_root: options[:source_root] || File.cwd!].update(options)

    source_beam = config.source_beam || Path.join(config.source_root, "ebin")
    docs = ExDoc.Retriever.get_docs find_beams(source_beam), config

    output = Path.expand(config.output)
    File.mkdir_p output

    formatter = config.formatter
    generate_index(formatter, output, config)
    generate_assets(formatter, output, config)
    Enum.each docs, fn({ name, nodes }) ->
      generate_list name, nodes, formatter, output, config
    end
  end

  # Helpers

  defp normalize_options(options) do
    pattern = options[:source_url_pattern] || guess_url(options[:source_url])
    Keyword.put(options, :source_url_pattern, pattern)
  end

  defp guess_url(url = <<"https://github.com/", _ :: binary>>) do
    append_slash(url) <> "blob/master/%{path}#L%{line}"
  end

  defp guess_url(url = <<"https://bitbucket.org/", _ :: binary>>) do
    append_slash(url) <> "src/master/%{path}?at=master#cl-%{line}"
  end

  defp guess_url(other) do
    other
  end

  defp append_slash(url) do
    if :binary.last(url) == ?/, do: url, else: url <> "/"
  end

  defp find_beams(path) do
    Path.wildcard Path.expand("Elixir-*.beam", path)
  end

  defp generate_index(formatter, output, config) do
    content = formatter.index_page(config)
    File.write("#{output}/index.html", content)
  end

  defp generate_assets(formatter, output, _config) do
    Enum.each formatter.assets, fn({ pattern, dir }) ->
      output = "#{output}/#{dir}"
      File.mkdir output

      Enum.map Path.wildcard(pattern), fn(file) ->
        base = Path.basename(file)
        File.copy file, "#{output}/#{base}"
      end
    end
  end

  defp generate_list(scope, nodes, formatter, output, config) do
    generate_module_page(nodes, formatter, output, config)
    content = formatter.list_page(scope, nodes, config)
    File.write("#{output}/#{scope}_list.html", content)
  end

  defp generate_module_page([node|t], formatter, output, config) do
    content = formatter.module_page(node, config)
    File.write("#{output}/#{node.id}.html", content)

    generate_module_page(node.children, formatter, output, config)
    generate_module_page(t, formatter, output, config)
  end

  defp generate_module_page([], _formatter, _output, _config) do
    :ok
  end
end
