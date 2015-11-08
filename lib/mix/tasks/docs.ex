defmodule Mix.Tasks.Docs do
  use Mix.Task

  @shortdoc "Generate documentation for the project"
  @recursive true

  @moduledoc """
  Uses ExDoc to generate a static web page from the docstrings extracted from
  all of the project's modules.

  ## Command line options

  * `--output`, `-o` - output directory for the generated docs; default: `"doc"`

  ## Configuration

  The task uses the project's `:name` key if defined, otherwise it will use the
  `:app` key as a substitute.

  It also uses the `:version` key and `:source_url` from the project's configuration.

  The following options should be put under the `:docs` key in your project's
  main configuration. The docs options should be a keyword list or a function
  returning a keyword list that will be lazily executed.

  * `:output` - output directory for the generated docs; default: "doc".
    May be overriden by command line argument.

  * `:formatter` - doc formatter to use; default: "html".

  * `:source_root` - path to the source code root directory;
    default: "." (current directory).

  * `:source_beam` - path to the beam directory; default: mix's compile path.

  * `:source_url_pattern` - public URL of the project.
    Derived from project's `:source_url` if not present.

  * `:source_ref` - the branch/commit/tag used for source link inference.
    Ignored if `:source_url_pattern` is provided; default: master.

  * `:main` - main page of the documentation. It may be a module or a
    generated page, like "Plug" or "api-reference";
    default: "api-reference" when --formatter is "html".

  * `:logo` - Path to the image logo of the project (only PNG or JPEG accepted)
    The image size will be 64x64 when --formatter is "html".

  * `:extra_section` - String that define the section title of the additional
    Markdown pages (e.g. "GUIDES"); default: "PAGES"

  * `:extras` - List of keywords, each key must indicate the path to additional
    Markdown pages, the value for each keyword (optional) gives you more control
    about the PATH and the title of the output files, please remember that the
    title also will be used in the sidebar area (under the :extra_section); default: `[]`
    (e.g. `["README.md", "CONTRIBUTING.md": [path: "CONTRIBUTORS", title: "Help us!"]]`)
  """

  @doc false
  def run(args, config \\ Mix.Project.config, generator \\ &ExDoc.generate_docs/3) do
    Mix.Task.run "compile"

    {cli_opts, args, _} = OptionParser.parse(args,
                            aliases: [o: :output],
                            switches: [output: :string])

    if args != [] do
      Mix.raise "Extraneous arguments on the command line"
    end

    project =
      (config[:name] || config[:app])
      |> to_string()
    version = config[:version] || "dev"
    options = Keyword.merge(get_docs_opts(config), cli_opts)

    if config[:source_url] do
      options = Keyword.put(options, :source_url, config[:source_url])
    end

    main = options[:main]
    options =
      cond do
        is_nil(main) ->
          Keyword.delete(options, :main)

        is_atom(main) ->
          Keyword.put(options, :main, inspect(main))

        is_binary(main)->
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
