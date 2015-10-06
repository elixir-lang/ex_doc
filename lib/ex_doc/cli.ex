defmodule ExDoc.CLI do
  def run(args, generator \\ &ExDoc.generate_docs/3) do
    {opts, args, _} = OptionParser.parse(args,
               aliases: [o: :output, f: :formatter, c: :config, r: :source_root,
                         u: :source_url, m: :main, p: :homepage_url, l: :logo,
                         e: :extra, v: :version],
               switches: [extra: :keep])

    cond do
      List.keymember?(opts, :version, 0) ->
        do_version
      true ->
        do_generate(args, opts, generator)
    end
  end

  defp do_version do
    IO.puts "ExDoc v#{ExDoc.version}"
  end

  defp do_generate(args, opts, generator) do
    [project, version, source_beam] = parse_args(args)

    Code.prepend_path(source_beam)
    opts =
      opts
      |> Keyword.put(:source_beam, source_beam)
      |> merge_config()

    generator.(project, version, opts)
  end

  defp merge_config(opts) do
    opts
    |> formatter_options()
    |> extra_files_options()
  end

  defp formatter_options(opts) do
    case Keyword.fetch(opts, :config) do
      {:ok, config} ->
        opts
        |> Keyword.delete(:config)
        |> Keyword.put(:formatter_opts, read_config(config))
      _ -> opts
    end
  end

  defp extra_files_options(opts) do
    extras = Keyword.get_values(opts, :extra)

    opts
    |> Keyword.delete(:extra)
    |> Keyword.put(:extras, extras)
  end

  defp read_config(path) do
    config = File.read!(path)
    {result, _} = Code.eval_string(config)

    unless is_list(result) do
      raise "expected a keyword list from config file: #{inspect path}"
    end

    result
  end

  defp parse_args([_project, _version, _source_beam] = args), do: args

  defp parse_args([_, _, _ | _]) do
    IO.puts "Too many arguments.\n"
    print_usage()
    exit {:shutdown, 1}
  end

  defp parse_args(_) do
    IO.puts "Too few arguments.\n"
    print_usage()
    exit {:shutdown, 1}
  end

  defp print_usage do
    IO.puts ~S"""
    Usage:
      ex_doc PROJECT VERSION BEAMS [OPTIONS]

    Examples:
      ex_doc "Dynamo" "0.8.0" "_build/shared/lib/dynamo/ebin" -u "https://github.com/elixir-lang/dynamo"

    Options:
      PROJECT             Project name
      VERSION             Version number
      BEAMS               Path to compiled beam files
      -c, --config        Give configuration through a file instead of command line
      -o, --output        Path to output docs, default: "doc"
      -f, --formatter     Docs formatter to use, default: "html"
      -r, --source-root   Path to the source code root, default: "."
      -u, --source-url    URL to the source code
          --source-ref    Branch/commit/tag used for source link inference, default: "master"
      -m, --main          The main, entry-point module in docs,
                          default: "extra-api-reference" when --formatter is "html"
      -p, --homepage-url  URL to link to for the site name
      -e, --extra         Allow users to include additional Markdown files
                          May be given multiple times
      -l, --logo          Path to the image logo of the project (only PNG or JPEG accepted)
                          The image size will be 64x64 when --formatter is "html"
                          default: `nil`
      -v, --version       Print ExDoc version

    ## Source linking

    ExDoc by default provide links to the source code implementation as
    long as `--source-url` or `--source-url-pattern` is provided. If you
    provide `--source-url`, ExDoc will inflect the url pattern automatically
    for GitHub and Bitbucket URLs. For example:

        --source-url "https://github.com/elixir-lang/dynamo"

    Will be inflected as:

        https://github.com/elixir-lang/dynamo/blob/master/%{path}#L%{line}

    To specify a particular branch or commit, use the `--source-ref` option:

        --source-url "https://github.com/elixir-lang/dynamo" --source-ref "v1.0"

    will result in the following URL pattern:

        https://github.com/elixir-lang/dynamo/blob/v1.0/%{path}#L%{line}

    """
  end
end
