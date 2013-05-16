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

  It also uses the `:version` key and `:source_url` from the project's configuration.

  The following options should be put under the `:docs` key in your project's
  main configuration.

  * `:output` - output directory for the generated docs; default: docs.
    May be overriden by command line argument.

  * `:formatter` - doc formatter to use; default: ExDoc.HTMLFormatter.

  * `:source_root` - path to the source code root directory; default: . (current directory).

  * `:source_url_pattern` - public URL of the project.
    Derived from project's `:source_url` if not present.

  * `:source_ref` - the branch/commit/tag used for source link inference.
    Ignored if `:source_url_pattern` is provided. Defaults to _master_.

  * `:main` - main module of the project, will be shown on the starting page.
    Derived from project's `:app` if not present.

  """

  def run(args) do
    Mix.Task.run "compile"

    { cli_opts, args } = OptionParser.parse(args, aliases: [o: :output])

    if args != [] do
      raise Mix.Error, message: "Extraneous arguments on the command line"
    end

    project = (Mix.project[:name] || Mix.project[:app]) |> to_binary
    version = Mix.project[:version] || "dev"
    options = Mix.project[:docs] || []

    cond do
      nil?(options[:main]) ->
        # Try generating main module's name from the app name
        options = Keyword.put(options, :main, (Mix.project[:app] |> atom_to_binary |> Mix.Utils.camelize))

      is_atom(options[:main]) ->
        options = Keyword.update(options, :main, Module.to_binary(&1))
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
        raise Mix.Error, message: "Unrecognized option: #{to_binary opt}"
      end
    end

    ExDoc.generate_docs(project, version, options)

    index = Path.join(options[:output] || "docs", "index.html")
    Mix.shell.info "%{green}Docs generated with success."
    Mix.shell.info "%{green}Open up #{index} in your browser to read them."
  end
end
