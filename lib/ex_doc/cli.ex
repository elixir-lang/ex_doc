defmodule ExDoc.CLI do
  @moduledoc """
  Command line parser for ExDoc
  """

  @doc """
  Handles the command line parsing and triggers the required mechanism to
  transform the Markdown documents into a specified format (default is HTML).
  """
  def main(args, generator \\ &ExDoc.generate_docs/3) do
    {opts, args, _invalid} =
      OptionParser.parse(args,
        aliases: [
          a: :assets,
          n: :canonical,
          c: :config,
          e: :extra,
          s: :extra_section,
          i: :filter_prefix,
          f: :formatter,
          p: :homepage_url,
          l: :logo,
          m: :main,
          o: :output,
          r: :source_root,
          u: :source_url,
          v: :version,
        ],

        switches: [
          extra: :keep,
          language: :string,
          source_ref: :string,
          version: :boolean,
        ],
      )

    if List.keymember?(opts, :version, 0) do
      print_version()
    else
      generate(args, opts, generator)
    end
  end

  defp print_version do
    IO.puts "ExDoc v#{ExDoc.version}"
  end

  defp generate(args, opts, generator) do
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
      ex_doc "Ecto" "0.8.0" "_build/dev/lib/ecto/ebin" -u "https://github.com/elixir-lang/ecto"
      ex_doc "Project" "1.0.0" "_build/dev/lib/project/ebin" -e "docs/extra_page.md"

    Options:
      PROJECT             Project name
      VERSION             Version number
      BEAMS               Path to compiled beam files
      -a, --assets        Path to a directory that will be copied as is to the "assets"
                          directory in the output path
      -n, --canonical     Indicate the preferred URL with rel="canonical" link element
      -c, --config        Give configuration through a file instead of command line
      -e, --extra         Allow users to include additional Markdown files
                          May be given multiple times
      -s, --extra-section Allow user to define the title for the additional Markdown files,
                          default: "PAGES"
      -i, --filter-prefix Include only modules that match the given prefix in
                          the generated documentation.
      -f, --formatter     Docs formatter to use (html or epub), default: "html"
      -p, --homepage-url  URL to link to for the site name
          --language      Identify the primary language of the documents, its value must be
                          a valid [BCP 47](https://tools.ietf.org/html/bcp47) language tag, default: "en"
      -l, --logo          Path to the image logo of the project (only PNG or JPEG accepted)
                          The image size will be 64x64 and copied to the assets directory
      -m, --main          The entry-point page in docs, default: "api-reference"
          --source-ref    Branch/commit/tag used for source link inference, default: "master"
      -r, --source-root   Path to the source code root, default: "."
      -u, --source-url    URL to the source code
      -o, --output        Path to output docs, default: "doc"
      -v, --version       Print ExDoc version

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

    """
  end
end
