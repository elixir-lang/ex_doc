defmodule Mix.Tasks.Docs do
  use Mix.Task

  @shortdoc "Generate HTML documentation for the project"
  @recursive true
  @moduledoc """
  Uses ExDoc to generate a static web page from the docstrings extracted from
  all of the project's modules.

  ## Command line options

  * `--output`, `-o` - output directory for the generated docs; default: `"docs"`

  ## Configuration

  The task uses the project's `:name` key if defined, otherwise it will use the
  `:app` key as a substitute.

  It also uses the `:version` key and `:source_url` from the project's configuration.

  The following options should be put under the `:docs` key in your project's
  main configuration. The docs options should be a keyword list or a function
  returning a keyword list that will be lazily executed.

  * `:output` - output directory for the generated docs; default: docs.
    May be overriden by command line argument.

  * `:readme` - boolean indicating whether a project README should be created
    from a README.md; default: `false`.

  * `:formatter` - doc formatter to use; default: html.

  * `:source_root` - path to the source code root directory; default: . (current directory).

  * `:source_beam` - path to the beam directory; default: mix's compile path.

  * `:source_url_pattern` - public URL of the project.
    Derived from project's `:source_url` if not present.

  * `:source_ref` - the branch/commit/tag used for source link inference.
    Ignored if `:source_url_pattern` is provided; default: master.

  * `:main` - main page of the documentation. It may be a module or a
    generated page, like "overview" or "README";
  """

  @doc false
  def run(args, config \\ Mix.Project.config, generator \\ &ExDoc.generate_docs/3) do
    Mix.Task.run "compile"

    { cli_opts, args, _ } = OptionParser.parse(args, aliases: [o: :output], switches: [output: :string])

    if args != [] do
      raise Mix.Error, message: "Extraneous arguments on the command line"
    end

    project = (config[:name] || config[:app]) |> to_string
    version = config[:version] || "dev"
    options = Keyword.merge(get_docs_opts(config), cli_opts)

    if source_url = config[:source_url] do
      options = Keyword.put(options, :source_url, source_url)
    end

    cond do
      is_nil(options[:main]) ->
        # Try generating main module's name from the app name
        options = Keyword.put(options, :main, (config[:app] |> Atom.to_string |> Mix.Utils.camelize))

      is_atom(options[:main]) ->
        options = Keyword.update!(options, :main, &inspect/1)

      is_binary(options[:main]) ->
        options
    end

    options = Keyword.put_new(options, :source_beam, Mix.Project.compile_path)

    index = generator.(project, version, options)
    log(index)
    index
  end

  defp log(index) do
    Mix.shell.info [:green, "Docs successfully generated."]
    Mix.shell.info [:green, "View them at #{inspect index}."]
  end

  defp get_docs_opts(config) do
    docs = config[:docs]
    cond do
      is_function(docs, 0) -> docs.()
      is_nil(docs) -> []
      true -> docs
    end
  end
end
