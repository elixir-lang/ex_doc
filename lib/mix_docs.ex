defmodule Mix.Tasks.Docs do
  use Mix.Task

  @shortdoc "Generate HTML documentation for the project"

  @moduledoc """
  There is a bunch of customizable options available. Put them under your project's :doc key.

  Your project must have :name defined. If you also define :version, it will be
  used. Otherwise, the version will be set to "dev".

    :output       output directory for the generated docs; default: docs
    :formatter    doc formatter to use; default: ExDoc.HTMLFormatter
    :source_root  path to the source code root directory; default: . (current directory)
    :source_url   public URL of the project (optional)
    :main         main module of the project, will be shown on the starting page


  Command-line usage:

    mix docs [-o/--output <path>]

  """

  def run(args) do
    { cli_opts, args } = OptionParser.parse(args, aliases: [o: :output])
    if args != [] do
      IO.puts "Extraneous arguments on the command line.\n"
      print_usage()
    end

    if nil?(project = Mix.project[:name]) do
      IO.puts "Undefined project name (:name option).\n"
      exit(1)
    end
    if nil?(version = Mix.project[:version]) do
      version = "dev"
    end

    options = Mix.project[:doc_options]
    if is_atom(options[:main]) do
      options = Keyword.update(options, :main, fn(mod) -> Module.to_binary(mod) end)
    end
    if formatter = options[:formatter] do
      options = Keyword.put(options, :formatter, String.split(formatter, "."))
    end

    # Merge command-line and project options
    options = Enum.reduce cli_opts, options, fn(opt, acc) ->
      if opt == :output do
        Keyword.put(acc, :output, opt)
      else
        { opt, _ } = opt
        IO.puts "Unrecognized option: #{to_binary opt}"
        exit(1)
      end
    end

    ExDoc.generate_docs(project, version, options)
  end

  defp print_usage do
    IO.puts %B"""
    Usage:
      mix docs [-o <path>]

      -o, --output      output directory for the generated docs; default: docs
    """
    exit(1)
  end
end
