defmodule ExDoc.CLI do
  def run(args) do
    parsed = OptionParser.parse(args, switches: [readme: :boolean],
               aliases: [o: :output, f: :formatter, u: :source_url, r: :source_root,
                         m: :main, p: :homepage_url])

    opts = elem(parsed, 0)
    args = elem(parsed, 1)

    case args do
      [project, version] -> :ok
      [_,_|_] ->
        IO.puts "Too many arguments.\n"
        print_usage()
      _ ->
        IO.puts "Too few arguments.\n"
        print_usage()
    end

    if formatter = opts[:formatter] do
      opts = Keyword.put(opts, :formatter, String.split(formatter, "."))
    end

    ExDoc.generate_docs(project, version, opts)
  end

  defp print_usage do
    IO.puts %B"""
    Usage:
      ex_doc PROJECT VERSION [OPTIONS]

    Examples:
      ex_doc "Dynamo" "0.8.0" -u "https://github.com/elixir-lang/dynamo"

    Options:
      -o, --output       Path to output docs, default: docs
      --readme           Generate a project README from a README.md file, default: false
      -f, --formatter    Docs formatter to use, default: ExDoc.HTMLFormatter
      -r, --source-root  Path to the source code root, default: .
      -u, --source-url   URL to the source code
      --source-ref       Branch/commit/tag used for source link inference, default: master
      -m, --main         The main, entry-point module in docs
      -p  --homepage-url URL to link to for the site name

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
    exit(1)
  end
end
