defmodule ExDoc do
  @moduledoc """
  Elixir Documentation System. ExDoc produces documentation for Elixir projects
  """

  defmodule Config do
    @moduledoc """
    Configuration structure that holds all the available options for ExDoc

    You can find more details about these options in the `ExDoc.CLI` module.
    """
    defstruct [
      assets: nil,
      canonical: nil,
      deps: [],
      extra_section: nil,
      extras: [],
      filter_prefix: nil,
      formatter: "html",
      formatter_opts: [],
      homepage_url: nil,
      logo: nil,
      main: nil,
      output: "doc",
      project: nil,
      retriever: ExDoc.Retriever,
      source_beam: nil,
      source_root: nil,
      source_url: nil,
      source_url_pattern: nil,
      title: nil,
      version: nil
    ]

     @type t :: %__MODULE__{
       assets: nil | String.t,
       canonical: nil | String.t,
       deps: [{ebin_path :: String.t, doc_url :: String.t}],
       extra_section: nil | String.t,
       extras: list(),
       filter_prefix: nil | String.t,
       formatter: nil | String.t,
       formatter_opts: Keyword.t,
       homepage_url: nil | String.t,
       logo: nil | Path.t,
       main: nil | String.t,
       output: nil | Path.t,
       project: nil | String.t,
       retriever: :atom,
       source_beam: nil | String.t,
       source_root: nil | String.t,
       source_url: nil | String.t,
       source_url_pattern: nil | String.t,
       title: nil | String.t,
       version: nil | String.t
     }
  end

  @ex_doc_version Mix.Project.config[:version]

  @doc """
  Returns the ExDoc version (used in templates).
  """
  @spec version :: String.t
  def version, do: @ex_doc_version

  @doc """
  Generates documentation for the given `project`, `vsn` (version)
  and `options`.
  """
  @spec generate_docs(String.t, String.t, Keyword.t) :: atom
  def generate_docs(project, vsn, options) when is_binary(project) and is_binary(vsn) and is_list(options) do
    config = build_config(project, vsn, options)
    docs = config.retriever.docs_from_dir(config.source_beam, config)
    find_formatter(config.formatter).run(docs, config)
  end

  # Builds configuration by merging `options`, and normalizing the options.
  @spec build_config(String.t, String.t, Keyword.t) :: ExDoc.Config.t
  defp build_config(project, vsn, options) do
    options = normalize_options(options)
    preconfig = %Config{
      project: project,
      version: vsn,
      main: options[:main],
      homepage_url: options[:homepage_url],
      source_root: options[:source_root] || File.cwd!,
    }
    struct(preconfig, options)
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
      Keyword.put(options, :output, String.trim_trailing(options[:output], "/"))
    else
      options
    end
  end

  defp guess_url(url = <<"https://github.com/", _ :: binary>>, ref) do
    append_slash(url) <> "blob/#{ref}/%{path}#L%{line}"
  end

  defp guess_url(url = <<"https://gitlab.com/", _ :: binary>>, ref) do
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
