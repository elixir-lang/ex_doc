defmodule ExDoc.CLI do
  @moduledoc false

  @doc """
  Handles the command line parsing for the escript.
  """
  def main(args, generator \\ &ExDoc.generate_docs/3) do
    {:ok, _} = Application.ensure_all_started(:ex_doc)

    {opts, args} =
      OptionParser.parse!(args,
        aliases: [
          c: :config,
          f: :formatter,
          l: :logo,
          m: :main,
          o: :output,
          q: :quiet,
          u: :source_url,
          v: :version
        ],
        switches: [
          canonical: :string,
          config: :string,
          formatter: [:keep, :string],
          homepage_url: :string,
          language: :string,
          logo: :string,
          main: :string,
          output: :string,
          package: :string,
          package: :string,
          paths: [:keep, :string],
          proglang: :string,
          quiet: :boolean,
          source_ref: :string,
          source_url: :string,
          version: :boolean
        ]
      )

    if List.keymember?(opts, :version, 0) do
      IO.puts("ExDoc v#{ExDoc.version()}")
    else
      generate(args, opts, generator)
    end
  end

  defp generate(args, opts, generator) do
    [project, version, source_beam] = parse_args(args)

    Code.prepend_path(source_beam)

    for path <- Keyword.get_values(opts, :paths),
        path <- Path.wildcard(path) do
      Code.prepend_path(path)
    end

    opts =
      opts
      |> Keyword.put(:source_beam, source_beam)
      |> Keyword.put(:apps, [app(source_beam)])
      |> merge_config()
      |> normalize_formatters()

    quiet? = Keyword.get(opts, :quiet, false)

    for formatter <- opts[:formatters] do
      index = generator.(project, version, Keyword.put(opts, :formatter, formatter))

      quiet? ||
        IO.puts(IO.ANSI.format([:green, "View #{inspect(formatter)} docs at #{inspect(index)}"]))

      index
    end
  end

  defp normalize_formatters(opts) do
    formatters =
      case Keyword.get_values(opts, :formatter) do
        [] -> opts[:formatters] || ["html", "epub"]
        values -> values
      end

    Keyword.put(opts, :formatters, formatters)
  end

  defp app(source_beam) do
    case Path.wildcard(Path.join([source_beam, "*.app"])) do
      [path] ->
        path |> Path.basename(".app") |> String.to_atom()

      _ ->
        raise "cannot find .app file in #{inspect(source_beam)}"
    end
  end

  defp merge_config(opts) do
    case Keyword.fetch(opts, :config) do
      {:ok, config} ->
        opts_without_config = Keyword.delete(opts, :config)
        Keyword.merge(read_config(config), opts_without_config)

      _ ->
        opts
    end
  end

  defp read_config(path) do
    case Path.extname(path) do
      ".exs" ->
        read_config_exs(path)

      ".config" ->
        read_config_erl(path)

      other ->
        raise "expected config to have .exs or .config extension, got: #{inspect(other)}"
    end
  end

  defp read_config_exs(path) do
    config = File.read!(path)
    {result, _} = Code.eval_string(config)

    unless is_list(result) do
      raise "expected a keyword list from config file: #{inspect(path)}"
    end

    result
  end

  defp read_config_erl(path) do
    case :file.consult(path) do
      {:ok, config} ->
        config

      {:error, reason} ->
        raise "error parsing #{path}: #{inspect(reason)}"
    end
  end

  defp parse_args([_project, _version, _source_beam] = args), do: args

  defp parse_args([_, _, _ | _]) do
    IO.puts("Too many arguments.\n")
    print_usage()
    exit({:shutdown, 1})
  end

  defp parse_args(_) do
    IO.puts("Too few arguments.\n")
    print_usage()
    exit({:shutdown, 1})
  end

  defp print_usage do
    IO.puts(~S"""
    Usage:
      ex_doc PROJECT VERSION BEAMS [OPTIONS]

    Examples:
      ex_doc "Ecto" "0.8.0" "_build/dev/lib/ecto/ebin" -u "https://github.com/elixir-ecto/ecto"
      ex_doc "Project" "1.0.0" "_build/dev/lib/project/ebin" -c "docs.exs"

    Options:
      PROJECT             Project name
      VERSION             Version number
      BEAMS               Path to compiled beam files
          --canonical     Indicate the preferred URL with rel="canonical" link element
      -c, --config        Give configuration through a file instead of a command line.
                          See "Custom config" section below for more information.
      -f, --formatter     Docs formatter to use (html or epub), default: html and epub
          --homepage-url  URL to link to for the site name
          --language      Identify the primary language of the documents, its value must be
                          a valid [BCP 47](https://tools.ietf.org/html/bcp47) language tag, default: "en"
      -l, --logo          Path to a logo image for the project. Must be PNG, JPEG or SVG. The image will
                          be placed in the output "assets" directory.
      -m, --main          The entry-point page in docs, default: "api-reference"
      -o, --output        Path to output docs, default: "doc"
          --package       Hex package name
          --paths         Prepends the given path to Erlang code path. The path might contain a glob
                          pattern but in that case, remember to quote it: --paths "_build/dev/lib/*/ebin".
                          This option can be given multiple times
          --proglang      The project's programming language, default: "elixir"
      -q, --quiet         Only output warnings and errors
          --source-ref    Branch/commit/tag used for source link inference, default: "master"
      -u, --source-url    URL to the source code
      -v, --version       Print ExDoc version

    ## Custom config

    A custom config can be given with the `--config` option.

    The file must either have ".exs" or ".config" extension.

    The file with the ".exs" extension must be an Elixir script that returns a keyword list with
    the same options declares in `Mix.Tasks.Docs`. Here is an example:

        [
          extras: Path.wildcard("lib/elixir/pages/*.md"),
          groups_for_docs: [
            Guards: & &1[:guard] == true
          ],
          skip_undefined_reference_warnings_on: ["compatibility-and-deprecations"],
          groups_for_modules: [
            ...
          ]
        ]

    The file with the ".config" extension must contain Erlang terms separated by ".".
    See `:file.consult/1` for more information. Here is an example:

        {extras, [<<"README.md">>, <<"CHANGELOG.md">>]}.
        {main, <<"readme">>}.
        {proglang, erlang}.

    ## Source linking

    ExDoc by default provides links to the source code implementation as
    long as `--source-url` or `--source-url-pattern` is provided. If you
    provide `--source-url`, ExDoc will inflect the url pattern automatically
    for GitHub, GitLab, and Bitbucket URLs. For example:

        --source-url "https://github.com/elixir-ecto/ecto"

    Will be inflected as:

        https://github.com/elixir-ecto/ecto/blob/master/%{path}#L%{line}

    To specify a particular branch or commit, use the `--source-ref` option:

        --source-url "https://github.com/elixir-ecto/ecto" --source-ref "v1.0"

    will result in the following URL pattern:

        https://github.com/elixir-ecto/ecto/blob/v1.0/%{path}#L%{line}

    """)
  end
end
