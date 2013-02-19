defmodule ExDoc.CLI do
  def run(args) do
    { options, args } = OptionParser.parse(args,
      aliases: [o: :output, f: :formatter, u: :source_url, r: :source_root, m: :main])

    case args do
      [project, version] -> :ok
      [_,_|_] ->
        IO.puts "Too many arguments.\n"
        print_usage()
      _ ->
        IO.puts "Too few arguments.\n"
        print_usage()
    end

    if formatter = options[:formatter] do
      options = Keyword.put(options, :formatter, String.split(formatter, "."))
    end

    ExDoc.generate_docs(project, version, options)
  end

  defp print_usage do
    IO.puts %B"""
    Usage:
      exdoc PROJECT VERSION [OPTIONS]

    Examples:
      exdoc "Dynamo" "0.8.0" -u "https://github.com/elixir-lang/dynamo/blob/master/%{path}#L%{line}"

    Options:
      -o, --output      Path to output docs, default: docs
      -f, --formatter   Docs formatter to use, default: ExDoc.HTMLFormatter
      -s, --source-root Path to the source code root, default: .
      -u, --source-url  URL to the source code
      -m, --main        The main, entry-point module in docs
    """
    exit(1)
  end
end
