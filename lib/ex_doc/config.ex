defmodule ExDoc.Config do
  @moduledoc false

  # Defaults
  @default_source_ref "main"
  def default_group_for_doc(metadata), do: metadata[:group]
  def filter_modules(_module, _metadata), do: true
  def before_closing_head_tag(_), do: ""
  def before_closing_footer_tag(_), do: ""
  def before_closing_body_tag(_), do: ""
  def annotations_for_docs(_), do: []
  def skip_undefined_reference_warnings_on(_string), do: false
  def skip_code_autolink_to(_string), do: false
  def source_url_pattern(_, _), do: nil

  defstruct annotations_for_docs: &__MODULE__.annotations_for_docs/1,
            api_reference: true,
            apps: [],
            assets: %{},
            authors: nil,
            before_closing_body_tag: &__MODULE__.before_closing_body_tag/1,
            before_closing_footer_tag: &__MODULE__.before_closing_footer_tag/1,
            before_closing_head_tag: &__MODULE__.before_closing_head_tag/1,
            canonical: nil,
            cover: nil,
            deps: [],
            docs_groups: [],
            extra_section: nil,
            extras: [],
            favicon: nil,
            filter_modules: &__MODULE__.filter_modules/2,
            footer: true,
            formatter: "html",
            formatters: [],
            group_for_doc: &__MODULE__.default_group_for_doc/1,
            groups_for_extras: [],
            groups_for_modules: [],
            homepage_url: nil,
            language: "en",
            logo: nil,
            main: nil,
            nest_modules_by_prefix: [],
            output: "./doc",
            package: nil,
            proglang: :elixir,
            project: nil,
            redirects: %{},
            retriever: ExDoc.Retriever,
            search: [%{name: "Default", help: "In-browser search", url: "search.html?q="}],
            skip_undefined_reference_warnings_on:
              &__MODULE__.skip_undefined_reference_warnings_on/1,
            skip_code_autolink_to: &__MODULE__.skip_code_autolink_to/1,
            source_beam: nil,
            source_ref: @default_source_ref,
            source_url: nil,
            source_url_pattern: &__MODULE__.source_url_pattern/2,
            title: nil,
            version: nil,
            warnings_as_errors: false

  @type t :: %__MODULE__{
          annotations_for_docs: (map() -> list()),
          api_reference: boolean(),
          apps: [atom()],
          assets: %{binary() => binary()},
          authors: nil | [String.t()],
          before_closing_body_tag: (atom() -> String.t()) | mfa() | map(),
          before_closing_footer_tag: (atom() -> String.t()) | mfa() | map(),
          before_closing_head_tag: (atom() -> String.t()) | mfa() | map(),
          canonical: nil | String.t(),
          cover: nil | Path.t(),
          deps: [{ebin_path :: String.t(), doc_url :: String.t()}],
          docs_groups: [String.t()],
          extra_section: nil | String.t(),
          extras: list(),
          favicon: nil | Path.t(),
          filter_modules: (module, map -> boolean),
          formatter: nil | String.t(),
          formatters: [String.t()],
          group_for_doc: (keyword() -> String.t() | nil),
          groups_for_extras: [{binary(), term()}],
          groups_for_modules: [{binary(), term()}],
          homepage_url: nil | String.t(),
          language: String.t(),
          logo: nil | Path.t(),
          main: nil | String.t(),
          nest_modules_by_prefix: [String.t()],
          output: nil | Path.t(),
          package: :atom | nil,
          project: nil | String.t(),
          redirects: %{optional(String.t()) => String.t()} | [{String.t(), String.t()}],
          retriever: atom(),
          search: [%{name: String.t(), help: String.t(), url: String.t()}],
          skip_undefined_reference_warnings_on: (String.t() -> boolean),
          skip_code_autolink_to: (String.t() -> boolean),
          source_beam: nil | String.t(),
          source_ref: nil | String.t(),
          source_url: nil | String.t(),
          source_url_pattern: (String.t(), integer() -> String.t() | nil),
          title: nil | String.t(),
          version: nil | String.t(),
          warnings_as_errors: boolean()
        }

  def build(project, vsn, options) do
    {output, options} = Keyword.pop(options, :output, "./doc")
    {nest_modules_by_prefix, options} = Keyword.pop(options, :nest_modules_by_prefix, [])
    {proglang, options} = Keyword.pop(options, :proglang, :elixir)
    {filter_modules, options} = Keyword.pop(options, :filter_modules, &filter_modules/2)

    options =
      if groups_for_functions = options[:groups_for_functions] do
        IO.warn(":groups_for_functions is deprecated, please use :groups_for_docs instead")
        Keyword.put_new(options, :groups_for_docs, groups_for_functions)
      else
        options
      end

    apps = Keyword.get(options, :apps, [])

    {groups_for_docs, options} = Keyword.pop(options, :groups_for_docs, [])
    {groups_for_extras, options} = Keyword.pop(options, :groups_for_extras, [])

    {groups_for_modules, options} =
      Keyword.pop(options, :groups_for_modules, default_groups_for_modules(apps))

    {default_group_for_doc, options} =
      Keyword.pop(options, :default_group_for_doc, &default_group_for_doc/1)

    {skip_undefined_reference_warnings_on, options} =
      Keyword.pop(
        options,
        :skip_undefined_reference_warnings_on,
        &skip_undefined_reference_warnings_on/1
      )

    {skip_code_autolink_to, options} =
      Keyword.pop(options, :skip_code_autolink_to, &skip_code_autolink_to/1)

    {source_url_pattern, options} =
      Keyword.pop_lazy(options, :source_url_pattern, fn ->
        guess_url(options[:source_url], options[:source_ref] || @default_source_ref)
      end)

    {search, options} = Keyword.pop(options, :search, [])

    preconfig = %__MODULE__{
      filter_modules: normalize_filter_modules(filter_modules),
      docs_groups: for({group, _} <- groups_for_docs, do: to_string(group)),
      group_for_doc: normalize_groups_for_docs(groups_for_docs, default_group_for_doc),
      groups_for_extras: normalize_groups(groups_for_extras),
      groups_for_modules:
        normalize_groups(
          # TODO: The default module groups must be returned by the language
          groups_for_modules ++ [Deprecated: &deprecated?/1, Exceptions: &exception?/1]
        ),
      homepage_url: options[:homepage_url],
      main: options[:main],
      nest_modules_by_prefix: normalize_nest_modules_by_prefix(nest_modules_by_prefix),
      output: normalize_output(output),
      proglang: normalize_proglang(proglang),
      project: project,
      search: normalize_search(search),
      skip_undefined_reference_warnings_on:
        normalize_skip_list_function(skip_undefined_reference_warnings_on),
      skip_code_autolink_to: normalize_skip_list_function(skip_code_autolink_to),
      source_url_pattern: normalize_source_url_pattern(source_url_pattern),
      version: vsn
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

  defp normalize_groups_for_docs(groups, default) do
    groups = normalize_groups(groups)

    fn metadata ->
      Enum.find_value(groups, fn {group, function} ->
        function.(metadata) && group
      end) || default.(metadata)
    end
  end

  defp normalize_groups(groups) do
    for {k, v} <- groups, do: {to_string(k), v}
  end

  defp deprecated?(metadata), do: metadata[:deprecated] != nil
  defp exception?(metadata), do: metadata[:kind] == :exception

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

  defp normalize_skip_list_function(strings) when is_list(strings),
    do: &(&1 in strings)

  defp normalize_skip_list_function(fun) when is_function(fun, 1),
    do: fun

  defp normalize_source_url_pattern(function) when is_function(function, 2), do: function
  defp normalize_source_url_pattern(nil), do: &source_url_pattern/2

  defp normalize_source_url_pattern(binary) when is_binary(binary) do
    case :binary.split(binary, "%{path}") do
      [left, right] ->
        case :binary.split(left, "%{line}") do
          [line_left, line_right] ->
            fn path, line ->
              line_left <> Integer.to_string(line) <> line_right <> path <> right
            end

          [_] ->
            case :binary.split(right, "%{line}") do
              [line_left, line_right] ->
                fn path, line ->
                  left <> path <> line_left <> Integer.to_string(line) <> line_right
                end

              [_] ->
                fn path, _ -> left <> path <> right end
            end
        end

      [_] ->
        case :binary.split(binary, "%{line}") do
          [left, right] ->
            fn _, line -> left <> Integer.to_string(line) <> right end

          [_] ->
            fn _, _ -> binary end
        end
    end
  end

  defp normalize_source_url_pattern(other) do
    raise ArgumentError,
          ":source_url_pattern must be a string, a two-arity function or nil, got: #{inspect(other)}"
  end

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

  defp default_groups_for_modules([_app]) do
    []
  end

  defp default_groups_for_modules(apps) do
    Enum.map(apps, fn app ->
      Application.load(app)
      {app, Application.spec(app, :modules)}
    end)
  end

  defp normalize_search([]) do
    [%{name: "Default", help: "In-browser search", url: "search.html?q="}]
  end

  defp normalize_search(search) when is_list(search) do
    Enum.map(search, fn
      %{packages: _, url: _} ->
        raise ArgumentError, "search must provide either :url or :packages, but not both"

      %{url: url} = engine when not is_binary(url) ->
        bad_search!(engine)

      %{name: name, help: help} = engine
      when is_binary(name) and is_binary(help) ->
        engine
        |> Map.delete(:packages)
        |> Map.put_new_lazy(:url, fn ->
          if packages = engine[:packages] do
            "https://hexdocs.pm/?packages=#{normalize_package_search(packages)}&q="
          else
            "search.html?q="
          end
        end)

      other ->
        bad_search!(other)
    end)
  end

  defp normalize_search(other) do
    raise ArgumentError, "search must be a list of maps, got: #{inspect(other)}"
  end

  defp normalize_package_search([]) do
    raise ArgumentError, ":packages requires a non-empty list"
  end

  defp normalize_package_search(packages) do
    packages
    |> Enum.map_join(",", fn
      package when is_atom(package) ->
        "#{package}:latest"

      {package, version} when is_atom(package) and is_binary(version) ->
        "#{package}:#{version}"

      other ->
        raise ArgumentError,
              "entries in :packages must be either a package name or a package-version tuple, got: #{inspect(other)}"
    end)
    |> URI.encode_www_form()
  end

  defp bad_search!(other) do
    raise ArgumentError,
          """
          search entries must be a map with:

            * required :name as string
            * required :help as string
            * optional :url as a string
            * optional :packages as a non-empty list of packages and versions

          got: #{inspect(other)}
          """
  end
end
