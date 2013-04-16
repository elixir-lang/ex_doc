defmodule Mix.Tasks.Docs do
  use Mix.Task

  @shortdoc "Generate HTML documentation for the project"

  @moduledoc """
  Uses ExDoc to generate a static web page from the docstrings extracted from
  all of the project's modules.

  ## Command line options

  * `--output`, `-o` - output directory for the generated docs; default: docs

  ## Configuration

  The task uses the project's `:name` key if defined, otherwise it will use the
  `:app` key as a substitute.

  It also uses the `:version` key.

  The following options should be put under the `:docs` key in your project's
  main configuration.

    :output       output directory for the generated docs; default: docs
    :formatter    doc formatter to use; default: ExDoc.HTMLFormatter
    :source_root  path to the source code root directory; default: . (current directory)
    :source_url   public URL of the project (optional)
    :main         main module of the project, will be shown on the starting page

  """

  def run(args) do
    { cli_opts, args } = OptionParser.parse(args, aliases: [o: :output])
    if args != [] do
      IO.puts "Extraneous arguments on the command line.\n"
      exit(1)
    end

    if nil?(project = Mix.project[:name]) do
      project = Mix.project[:app]
    end

    if nil?(version = Mix.project[:version]) do
      version = "dev"
    end

    options = Mix.project[:docs]
    if nil?(options) do
      options = []
    end

    cond do
      nil?(options[:main]) ->
        # Try generating main module's name from the app name
        options = Keyword.put(options, :main, (Mix.project[:app] |> atom_to_binary |> Mix.Utils.camelize))

      is_atom(options[:main]) ->
        options = Keyword.update(options, :main, fn(mod) -> Module.to_binary(mod) end)
    end

    if formatter = options[:formatter] do
      options = Keyword.put(options, :formatter, String.split(formatter, "."))
    end

    if pattern = options[:source_url_pattern] do
      options = Keyword.put(options, :source_url, pattern)
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
end
