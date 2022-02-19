defmodule ExDoc.Config do
  @moduledoc false

  # Defaults
  @default_source_ref "main"
  def filter_modules(_module, _metadata), do: true
  def before_closing_head_tag(_), do: ""
  def before_closing_body_tag(_), do: ""

  defstruct apps: [],
            api_reference: true,
            assets: nil,
            before_closing_head_tag: &__MODULE__.before_closing_head_tag/1,
            before_closing_body_tag: &__MODULE__.before_closing_body_tag/1,
            canonical: nil,
            nest_modules_by_prefix: [],
            deps: [],
            extra_section: nil,
            extras: [],
            filter_modules: &__MODULE__.filter_modules/2,
            formatter: "html",
            groups_for_extras: [],
            groups_for_modules: [],
            groups_for_functions: [],
            homepage_url: nil,
            javascript_config_path: "docs_config.js",
            language: "en",
            proglang: :elixir,
            logo: nil,
            cover: nil,
            main: nil,
            output: "./doc",
            project: nil,
            retriever: ExDoc.Retriever,
            source_beam: nil,
            source_ref: @default_source_ref,
            source_url: nil,
            source_url_pattern: nil,
            title: nil,
            version: nil,
            authors: nil,
            skip_undefined_reference_warnings_on: [],
            package: nil

  @type t :: %__MODULE__{
          apps: [atom()],
          api_reference: boolean(),
          assets: nil | String.t(),
          before_closing_head_tag: (atom() -> String.t()),
          before_closing_body_tag: (atom() -> String.t()),
          canonical: nil | String.t(),
          nest_modules_by_prefix: [String.t()],
          deps: [{ebin_path :: String.t(), doc_url :: String.t()}],
          extra_section: nil | String.t(),
          extras: list(),
          groups_for_extras: keyword(),
          filter_modules: (module, map -> boolean),
          formatter: nil | String.t(),
          homepage_url: nil | String.t(),
          javascript_config_path: nil | String.t(),
          language: String.t(),
          logo: nil | Path.t(),
          cover: nil | Path.t(),
          main: nil | String.t(),
          groups_for_modules: keyword(),
          groups_for_functions: keyword((keyword() -> boolean)),
          output: nil | Path.t(),
          project: nil | String.t(),
          retriever: atom(),
          source_beam: nil | String.t(),
          source_ref: nil | String.t(),
          source_url: nil | String.t(),
          source_url_pattern: nil | String.t(),
          title: nil | String.t(),
          version: nil | String.t(),
          authors: nil | [String.t()],
          skip_undefined_reference_warnings_on: [String.t()],
          package: :atom | nil
        }

  @spec build(String.t(), String.t(), Keyword.t()) :: ExDoc.Config.t()
  def build(project, vsn, options) do
    {output, options} = Keyword.pop(options, :output, "./doc")
    {groups_for_modules, options} = Keyword.pop(options, :groups_for_modules, [])
    {nest_modules_by_prefix, options} = Keyword.pop(options, :nest_modules_by_prefix, [])
    {proglang, options} = Keyword.pop(options, :proglang, :elixir)
    {filter_modules, options} = Keyword.pop(options, :filter_modules, &filter_modules/2)

    {source_url_pattern, options} =
      Keyword.pop_lazy(options, :source_url_pattern, fn ->
        guess_url(options[:source_url], options[:source_ref] || @default_source_ref)
      end)

    preconfig = %__MODULE__{
      project: project,
      version: vsn,
      main: options[:main],
      output: normalize_output(output),
      homepage_url: options[:homepage_url],
      proglang: normalize_proglang(proglang),
      source_url_pattern: source_url_pattern,
      nest_modules_by_prefix: normalize_nest_modules_by_prefix(nest_modules_by_prefix),
      groups_for_modules: normalize_groups_for_modules(groups_for_modules),
      filter_modules: normalize_filter_modules(filter_modules)
    }

    struct(preconfig, options)
  end

  defp normalize_output(output) do
    String.trim_trailing(output, "/")
  end

  defp normalize_proglang(binary) when is_binary(binary) do
    binary |> String.to_atom() |> normalize_proglang()
  end

  defp normalize_proglang(proglang) when proglang in [:elixir, :erlang] do
    proglang
  end

  defp normalize_proglang(proglang) do
    raise ArgumentError, "#{inspect(proglang)} is not supported"
  end

  defp normalize_groups_for_modules(groups_for_modules) do
    default_groups = [Deprecated: &deprecated?/1, Exceptions: &exception?/1]

    groups_for_modules ++
      Enum.reject(default_groups, fn {k, _} -> Keyword.has_key?(groups_for_modules, k) end)
  end

  defp deprecated?(metadata), do: metadata[:deprecated] != nil
  defp exception?(metadata), do: metadata[:__type__] == :exception

  defp normalize_nest_modules_by_prefix(nest_modules_by_prefix) do
    nest_modules_by_prefix
    |> Enum.map(&inspect_atoms/1)
    |> Enum.sort()
    |> Enum.reverse()
  end

  defp inspect_atoms(atom) when is_atom(atom), do: inspect(atom)
  defp inspect_atoms(binary) when is_binary(binary), do: binary

  defp normalize_filter_modules(string) when is_binary(string),
    do: normalize_filter_modules(Regex.compile!(string))

  defp normalize_filter_modules(%Regex{} = regex),
    do: fn module, _ -> Atom.to_string(module) =~ regex end

  defp normalize_filter_modules(fun) when is_function(fun, 2),
    do: fun

  defp guess_url(url, ref) do
    with {:ok, host_with_path} <- http_or_https(url),
         {:ok, pattern} <- known_pattern(host_with_path, ref) do
      "https://" <> append_slash(host_with_path) <> pattern
    else
      _ -> url
    end
  end

  defp http_or_https("http://" <> rest),
    do: {:ok, rest}

  defp http_or_https("https://" <> rest),
    do: {:ok, rest}

  defp http_or_https(_),
    do: :error

  defp known_pattern("github.com/" <> _, ref),
    do: {:ok, "blob/#{ref}/%{path}#L%{line}"}

  defp known_pattern("gitlab.com/" <> _, ref),
    do: {:ok, "blob/#{ref}/%{path}#L%{line}"}

  defp known_pattern("bitbucket.org/" <> _, ref),
    do: {:ok, "src/#{ref}/%{path}#cl-%{line}"}

  defp known_pattern(_host_with_path, _ref),
    do: :error

  defp append_slash(url) do
    if :binary.last(url) == ?/, do: url, else: url <> "/"
  end
end
