defmodule Mix.Tasks.Docs do
  use Mix.Task

  @shortdoc "Generate documentation for the project"
  @recursive true

  @moduledoc """
  Uses ExDoc to generate a static web page from the project documentation.

  ## Command line options

    * `--output`, `-o` - output directory for the generated
      docs, default: `"doc"`
    * `--canonical`, `-a` - indicate the preferred URL with
      rel="canonical" link element, default: nil

  The command line options have lower precedence than the options
  specified in your `mix.exs` file below.

  ## Configuration

  ExDoc will automatically pull in information from your project,
  like the application and version. However, you may want to set
  `:name`, `:source_url` and `:homepage_url` to have a nicer output
  from ExDoc, for example:

      def project do
        [app: :my_app,
         version: "0.1.0-dev",
         name: "My App",
         source_url: "https://github.com/USER/APP",
         homepage_url: "http://YOUR_PROJECT_HOMEPAGE",
         deps: deps,
         docs: [logo: "path/to/logo.png",
                extras: ["README.md", "CONTRIBUTING.md"]]]
      end

  ExDoc also allows configuration specific to the documentation to
  be set. The following options should be put under the `:docs` key
  in your project's main configuration. The `:docs` options should
  be a keyword list or a function returning a keyword list that will
  be lazily executed.

    * `:output` - output directory for the generated docs; default: "doc".
      May be overriden by command line argument.

    * `:formatter` - doc formatter to use; default: "html".

    * `:source_root` - path to the source code root directory;
      default: "." (current directory).

    * `:source_beam` - path to the beam directory; default: mix's compile path.

    * `:source_ref` - the branch/commit/tag used for source link inference;
      default: "master".

    * `:source_url_pattern` - public URL of the project. Derived from
      project's `:source_url` and `:source_ref`. Example:
      "https://github.com/USER/APP/blob/master/%{path}#L%{line}"

    * `:main` - main page of the documentation. It may be a module or a
      generated page, like "Plug" or "api-reference";
      default: "api-reference" when --formatter is "html".

    * `:logo` - Path to the image logo of the project (only PNG or JPEG accepted)
      The image size will be 64x64 when --formatter is "html".

    * `:extras` - List of keywords, each key must indicate the path to additional
      Markdown pages, the value for each keyword (optional) gives you more control
      about the PATH and the title of the output files; default: `[]`. Example:
      `["README.md", "CONTRIBUTING.md": [path: "CONTRIBUTORS", title: "Join us!"]]`

    * `:extra_section` - String that define the section title of the additional
      Markdown pages; default: "PAGES". Example: "GUIDES"

    * `:canonical` - String that define the preferred URL with the rel="canonical"
      element; default: nil
  """

  @doc false
  def run(args, config \\ Mix.Project.config, generator \\ &ExDoc.generate_docs/3) do
    Mix.Task.run "compile"

    {cli_opts, args, _} = OptionParser.parse(args,
                            aliases: [o: :output, a: :canonical],
                            switches: [output: :string, canonical: :string])

    if args != [] do
      Mix.raise "Extraneous arguments on the command line"
    end

    project = (config[:name] || config[:app]) |> to_string()
    version = config[:version] || "dev"
    options = Keyword.merge(cli_opts, get_docs_opts(config))

    options =
      if config[:source_url] do
        Keyword.put(options, :source_url, config[:source_url])
      else
        options
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
