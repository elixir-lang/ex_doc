defmodule ExDoc do
  defrecord Config, output: "docs", source_root: nil, source_url: nil, source_url_pattern: nil,
                    homepage_url: nil, source_beam: nil,
                    retriever: ExDoc.Retriever, formatter: ExDoc.HTMLFormatter,
                    project: nil, version: nil, main: nil, readme: false,
                    create_assets: true, create_index: true

  defrecord ProjectInfo, project: nil, version: nil, documented_modules: nil

  defexception InvalidProjectInfo, message: nil

  @doc """
  Generates documentation for the given `project`, `version`
  and `options`.
  """
  def generate_docs(project, version, options) when is_binary(project) and is_binary(version) and is_list(options) do
    options = normalize_options(options)
    config  = Config[project: project, version: version, main: options[:main] || project,
                     homepage_url: options[:homepage_url],
                     output: options[:output] || "docs",
                     source_root: options[:source_root] || File.cwd!].update(options)

    # Ensure the relevant directories have been created if the formatter
    # requires it or assets or the index need to be created.
    if config.formatter.needs_output_dir || config.create_assets || config.create_index do
      config = config.output(Path.expand(config.output))
      File.mkdir_p!(Path.join(config.output, config.project))
    end

    docs = config.retriever.docs_from_dir(config.source_beam, config)
    res = config.formatter.run(docs, config)
    
    # Needs to happen before creating the index as the index will just read this
    # file again. Yes, it's inefficient, but it slightly simplifies the code. :)
    if is_list(docs) do
      # In the tests the IdentityRetriever returns a tuple instead of a list of
      # modules which causes this call to fail. Hence the test specific (yuck)
      # if-test.
      save_project_info(Path.join([config.output, config.project, ".ex_doc_project_info"]),
                        config.project, config.version, docs)
    end

    if config.create_assets do
      config.formatter.create_assets(config)
    end

    if config.create_index do
      infos = Enum.map(Path.wildcard("#{config.output}/*/.ex_doc_project_info"),
                       &load_project_info/1)
      config.formatter.create_index(config, infos)
    end

    res
  end

  # Helpers

  defp normalize_options(options) do
    pattern = options[:source_url_pattern] || guess_url(options[:source_url], options[:source_ref] || "master")
    Keyword.put(options, :source_url_pattern, pattern)
    if options[:no_assets] do
      options = Keyword.put(Keyword.delete(options, :no_assets), :create_assets, false)
    end
    if options[:no_index] do
      options = Keyword.put(Keyword.delete(options, :no_assets), :create_index, false)
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

  defp save_project_info(file, project, version, mods) do
    # Ignore protocol implementation modules. We don't document them.
    mods = Enum.filter_map(mods, &(&1.type != :impl), &{&1.module, &1.type})
    info = [project: project, version: version, documented_modules: mods]
    File.write!(file, "# ExDoc generated project information, do not edit\n" <> inspect(info))
  end
  
  defp load_project_info(file) do
    quotation = File.read!(file) |> Code.string_to_quoted!() |> sanitize_quotation()
    case Macro.safe_term(quotation) do
      :ok ->
        { info, _ } = Code.eval_quoted(quotation)
        project = info[:project]
        if nil? project do
          raise InvalidProjectInfo, message:
            "Missing key 'project' in #{inspect file}"
        end
        version = info[:version]
        if nil? version do
          raise InvalidProjectInfo, message:
            "Missing key 'version' in #{inspect file}"
        end
        mods = info[:documented_modules]
        if nil? mods do
          raise InvalidProjectInfo, message:
            "Missing key 'documented_modules' in #{inspect file}"
        end
        Enum.each(mods, fn
          { name, nil } when is_atom(name) -> :ok
          { name, type } when is_atom(name) and is_atom(type) -> :ok
          mod ->
            raise InvalidProjectInfo, message:
              "Invalid documented module specification in #{inspect file}: #{inspect mod}"
        end)
        ProjectInfo[project: project, version: version, documented_modules: mods]
      { :unsafe, term } ->
        raise InvalidProjectInfo, message:
          "Unsafe term in #{inspect file}: #{inspect term}" 
    end
  end

  defp sanitize_quotation({:__block__, _, [ast]}), do: ast
  defp sanitize_quotation(ast), do: ast

end
