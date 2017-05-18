defmodule ExDoc do
  @moduledoc """
  Elixir Documentation System. ExDoc produces documentation for Elixir projects
  """

  defmodule Config do
    @moduledoc """
    Configuration structure that holds all the available options for ExDoc

    You can find more details about these options in the `ExDoc.CLI` module.
    """

    @default %{
      :formatter => "html",
      :language => "en",
      :output => "./doc",
      :source_ref => "master",
      :retriever => ExDoc.Retriever,
    }

    @spec default(atom) :: term
    def default(field) do
      Map.fetch!(@default, field)
    end

    defstruct [
      assets: nil,
      canonical: nil,
      deps: [],
      extra_section: nil,
      extras: [],
      filter_prefix: nil,
      formatter: @default.formatter,
      formatter_opts: [],
      homepage_url: nil,
      language: @default.language,
      logo: nil,
      main: nil,
      output: @default.output,
      project: nil,
      retriever: @default.retriever,
      source_beam: nil,
      source_ref: @default.source_ref,
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
       language: String.t,
       logo: nil | Path.t,
       main: nil | String.t,
       output: nil | Path.t,
       project: nil | String.t,
       retriever: :atom,
       source_beam: nil | String.t,
       source_ref: nil | String.t,
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

  @doc false
  # Builds configuration by merging `options`, and normalizing the options.
  @spec build_config(String.t, String.t, Keyword.t) :: ExDoc.Config.t
  def build_config(project, vsn, options) do
    options = normalize_options(options)

    preconfig = %Config{
      project: project,
      version: vsn,
      source_root: options[:source_root] || File.cwd!,
    }
    struct(preconfig, options)
  end

  # Helpers

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

  @doc false
  # Removes trailing slash from paths
  def normalize_paths(options, fields) when is_list(fields) do
    Enum.reduce(fields, options, fn(field, options_acc) ->
      if is_binary(options_acc[field]) do
        Keyword.put(options_acc, field, String.trim_trailing(options_acc[field], "/"))
      else
        options_acc
      end
    end)
  end

  @doc false
  def normalize_source_url_pattern(options, default_source_ref \\ ExDoc.Config.default(:source_ref)) do
    if !options[:source_url_pattern] && options[:source_url] do
      source_ref = options[:source_ref] || default_source_ref
      Keyword.put(options, :source_url_pattern, guess_url(options[:source_url], source_ref))
    else
      options
    end
  end

  @doc false
  def normalize_options(options) do
    options = normalize_options(options, :source_ref, ExDoc.Config.default(:source_ref))

    options
    |> normalize_paths([:assets, :output, :source_root])
    |> normalize_source_url_pattern(options[:source_ref] || ExDoc.Config.default(:source_ref))
  end

  @doc false
  # Updates `field` in `options` with `default` value if current value is `nil` or hasn't been set,
  # and if `default` is not `nil`.
  def normalize_options(options, field, default) do
    if is_nil(options[field]) and not is_nil(default) do
      Keyword.put(options, field, default)
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
