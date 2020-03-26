defmodule ExDoc.CLI do
  @moduledoc false

  @doc """
  Handles the command line parsing for the escript.
  """
  def main(args, generator \\ &ExDoc.generate_docs/3) do
    {:ok, _} = Application.ensure_all_started(:ex_doc)

    {opts, args, _invalid} =
      OptionParser.parse(args,
        aliases: [
          n: :canonical,
          c: :config,
          f: :formatter,
          p: :homepage_url,
          l: :logo,
          m: :main,
          o: :output,
          r: :source_root,
          u: :source_url,
          v: :version
        ],
        switches: [
          language: :string,
          source_ref: :string,
          version: :boolean
        ]
      )

    if List.keymember?(opts, :version, 0) do
      print_version()
    else
      generate(args, opts, generator)
    end
  end

  defp print_version do
    IO.puts("ExDoc v#{ExDoc.version()}")
  end

  defp generate(args, opts, generator) do
    [project, version, source_beam] = parse_args(args)

    Code.prepend_path(source_beam)

    opts =
      opts
      |> Keyword.put(:source_beam, source_beam)
      |> Keyword.put(:app, app(source_beam))
      |> merge_config()

    generator.(project, version, opts)
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
        opts
        |> Keyword.delete(:config)
        |> Keyword.merge(read_config(config))

      _ ->
        opts
    end
  end

  defp read_config(path) do
    config = File.read!(path)
    {result, _} = Code.eval_string(config)

    unless is_list(result) do
      raise "expected a keyword list from config file: #{inspect(path)}"
    end

    result
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
      ex_doc "Ecto" "0.8.0" "_build/dev/lib/ecto/ebin" -u "https://github.com/elixir-lang/ecto"
      ex_doc "Project" "1.0.0" "_build/dev/lib/project/ebin" -c "docs.exs"

    Options:
      PROJECT             Project name
      VERSION             Version number
      BEAMS               Path to compiled beam files
      -n, --canonical     Indicate the preferred URL with rel="canonical" link element
      -c, --config        Give configuration through a file instead of command line
      -f, --formatter     Docs formatter to use (html or epub), default: "html"
      -p, --homepage-url  URL to link to for the site name
          --language      Identify the primary language of the documents, its value must be
                          a valid [BCP 47](https://tools.ietf.org/html/bcp47) language tag, default: "en"
      -l, --logo          Path to the image logo of the project (only PNG or JPEG accepted)
                          The image size will be 64x64 and copied to the assets directory
      -m, --main          The entry-point page in docs, default: "api-reference"
          --source-ref    Branch/commit/tag used for source link inference, default: "master"
      -r, --source-root   Path to the source code root, used for generating links, default: "."
      -u, --source-url    URL to the source code
      -o, --output        Path to output docs, default: "doc"
      -v, --version       Print ExDoc version

    ## Custom config

    A custom config can be given with the `--config` option. The file must
    be an Elixir script that returns a keyword list with the same options
    declare in `Mix.Tasks.Docs`.

        [
          extras: Path.wildcard("lib/elixir/pages/*.md"),
          groups_for_functions: [
            Guards: & &1[:guard] == true
          ],
          skip_undefined_reference_warnings_on: ["compatibility-and-deprecations"],
          groups_for_modules: [
            ...
          ]
        ]

    ## Source linking

    ExDoc by default provide links to the source code implementation as
    long as `--source-url` or `--source-url-pattern` is provided. If you
    provide `--source-url`, ExDoc will inflect the url pattern automatically
    for GitHub, GitLab, and Bitbucket URLs. For example:

        --source-url "https://github.com/elixir-lang/ecto"

    Will be inflected as:

        https://github.com/elixir-lang/ecto/blob/master/%{path}#L%{line}

    To specify a particular branch or commit, use the `--source-ref` option:

        --source-url "https://github.com/elixir-lang/ecto" --source-ref "v1.0"

    will result in the following URL pattern:

        https://github.com/elixir-lang/ecto/blob/v1.0/%{path}#L%{line}

    """)
  end
end
