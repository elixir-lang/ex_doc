defmodule Mix.Tasks.Docs do
  use Mix.Task

  @shortdoc "Generate documentation for the project"

  @moduledoc ~S"""
  Uses ExDoc to generate a static web page from the project documentation.

  ## Command line options

    * `--canonical`, `-n` - Indicate the preferred URL with
      rel="canonical" link element, defaults to no canonical path

    * `--formatter`, `-f` - Which formatters to use, "html" or
      "epub", default: "html" (may be given more than once)

    * `--output`, `-o` - Output directory for the generated
      docs, default: `"doc"`

  The command line options have higher precedence than the options
  specified in your `mix.exs` file below.

  ## Configuration

  ExDoc will automatically pull in information from your project,
  like the application and version. However, you may want to set
  `:name`, `:source_url` and `:homepage_url` to have a nicer output
  from ExDoc, for example:

      def project do
        [app: :my_app,
         version: "0.1.0-dev",
         deps: deps(),

         # Docs
         name: "My App",
         source_url: "https://github.com/USER/PROJECT",
         homepage_url: "http://YOUR_PROJECT_HOMEPAGE",
         docs: [main: "MyApp", # The main page in the docs
                logo: "path/to/logo.png",
                extras: ["README.md"]]]
      end

  ExDoc also allows configuration specific to the documentation to
  be set. The following options should be put under the `:docs` key
  in your project's main configuration. The `:docs` options should
  be a keyword list or a function returning a keyword list that will
  be lazily executed.

    * `:assets` - Path to a directory that will be copied as is to the "assets"
      directory in the output path. Its entries may be referenced in your docs
      under "assets/ASSET.EXTENSION"; defaults to no assets directory.

    * `:before_closing_body_tag` - a function that takes as argument an atom specifying
      the formatter being used (`:html` or `:epub`) and returns a literal HTML string
      to be included just before the closing body tag (`</body>`).
      The atom given as argument can be used to include different content in both formats.
      Useful to inject custom assets, such as Javascript.

    * `:before_closing_head_tag` - a function that takes as argument an atom specifying
      the formatter being used (`:html` or `:epub`) and returns a literal HTML string
      to be included just before the closing head tag (`</head>`).
      The atom given as argument can be used to include different content in both formats.
      Useful to inject custom assets, such as CSS stylesheets.

    * `:canonical` - String that defines the preferred URL with the rel="canonical"
      element; defaults to no canonical path.

    * `:deps` - A keyword list application names and their documentation URL.
      ExDoc will by default include all dependencies and assume they are hosted on
      HexDocs. This can be overridden by your own values. Example: `[plug: "https://myserver/plug/"]`

    * `:extra_section` - String that defines the section title of the additional
      Markdown pages; default: "PAGES". Example: "GUIDES"

    * `:extras` - List of keywords, each key must indicate the path to additional
      Markdown pages, the value for each keyword (optional) gives you more control
      about the PATH and the title of the output files; default: `[]`. Example:
      `["README.md", "CONTRIBUTING.md": [filename: "contributing", title: "Contributing"]]`

    * `:filter_prefix` - Include only modules that match the given prefix in
      the generated documentation. Example: "MyApp.Core"

    * `:formatters` - Formatter to use; default: ["html"], options: "html", "epub".

    * `:groups_for_extras`, `:groups_for_modules` - See next section

    * `:language` - Identify the primary language of the documents, its value must be
      a valid [BCP 47](https://tools.ietf.org/html/bcp47) language tag; default: "en"

    * `:logo` - Path to the image logo of the project (only PNG or JPEG accepted)
      The image size will be 64x64. When specified, the logo will be placed under
      the "assets" directory in the output path under the name "logo" and the
      appropriate extension.

    * `:main` - Main page of the documentation. It may be a module or a
      generated page, like "Plug" or "api-reference"; default: "api-reference".

    * `:markdown_processor` - The markdown processor to use;

    * `:markdown_processor_options` - Configuration options for the markdown processor;

    * `:source_beam` - Path to the beam directory; default: mix's compile path.

    * `:source_ref` - The branch/commit/tag used for source link inference;
      default: "master".

    * `:source_url_pattern` - Public URL of the project. Derived from
      project's `:source_url` and `:source_ref`. Example:
      "https://github.com/USER/APP/blob/master/%{path}#L%{line}"

    * `:output` - Output directory for the generated docs; default: "doc".
      May be overridden by command line argument.
    
    *`:ignore_apps` - Apps to be ignored when generating documentation in an umbrella project.
      Receives a list of atoms. Example: `[:first_app, :second_app]`.

  ## Groups

  ExDoc content can be organized in groups. This is done via the `:groups_for_extras`
  and `:groups_for_modules`. For example, imagine you are storing extra guides in
  your documentation which are organized per directory. In the extras section you
  have:

      extras: [
        "guides/introduction/foo.md",
        "guides/introduction/bar.md",

        ...

        "guides/advanced/baz.md",
        "guides/advanced/bat.md",
      ]

  You can have those grouped as follows:

      groups_for_extras: [
        "Introduction": Path.wildcard("guides/introduction/*.md"),
        "Advanced": Path.wildcard("guides/advanced/*.md")
      ]

  Or via a regex:

      groups_for_extras: [
        "Introduction": ~r"/introduction/"
        "Advanced": ~r"/advanced/"
      ]

  Similar can be done for modules:

      groups_for_modules: [
        "Data types": [Atom, Regex, URI],
        "Collections": [Enum, MapSet, Stream],
      ]

  A regex or the string name of the module is also supported.

  ## Umbrella project

  ExDoc can be used in an umbrella project and generates a single documentation
  for all child apps. You can use the `:ignore_apps` configuration to exclude
  certain projects in the umbrella from documentation.

  Generating documentation per each child app can be achieved by running:

      mix cmd mix docs

  See `mix help cmd` for more information.
  """

  @switches [
    canonical: :string,
    formatter: :keep,
    language: :string,
    output: :string
  ]

  @aliases [n: :canonical, f: :formatter, o: :output]

  @doc false
  def run(args, config \\ Mix.Project.config(), generator \\ &ExDoc.generate_docs/3) do
    Mix.Task.run("compile")
    {cli_opts, args, _} = OptionParser.parse(args, aliases: @aliases, switches: @switches)

    if args != [] do
      Mix.raise("Extraneous arguments on the command line")
    end

    project = to_string(config[:name] || config[:app])
    version = config[:version] || "dev"

    options =
      config
      |> get_docs_opts()
      |> Keyword.merge(cli_opts)
      # accepted at root level config
      |> normalize_source_url(config)
      # accepted at root level config
      |> normalize_homepage_url(config)
      |> normalize_source_beam(config)
      |> normalize_main()
      |> normalize_deps()

    for formatter <- get_formatters(options) do
      index = generator.(project, version, Keyword.put(options, :formatter, formatter))
      log(index)
      index
    end
  end

  defp get_formatters(options) do
    case Keyword.get_values(options, :formatter) do
      [] -> options[:formatters] || [ExDoc.Config.default_formatter()]
      values -> values
    end
  end

  defp get_docs_opts(config) do
    docs = config[:docs]

    cond do
      is_function(docs, 0) -> docs.()
      is_nil(docs) -> []
      true -> docs
    end
  end

  defp log(index) do
    Mix.shell().info([:green, "Docs successfully generated."])
    Mix.shell().info([:green, "View them at #{inspect(index)}."])
  end

  defp normalize_source_url(options, config) do
    if source_url = config[:source_url] do
      Keyword.put(options, :source_url, source_url)
    else
      options
    end
  end

  defp normalize_homepage_url(options, config) do
    if homepage_url = config[:homepage_url] do
      Keyword.put(options, :homepage_url, homepage_url)
    else
      options
    end
  end

  defp normalize_source_beam(options, config) do
    compile_path =
      if Mix.Project.umbrella?(config) do
        umbrella_compile_paths(Keyword.get(options, :ignore_apps, []))
      else
        Mix.Project.compile_path()
      end

    Keyword.put_new(options, :source_beam, compile_path)
  end

  defp umbrella_compile_paths(ignored_apps) do
    build = Mix.Project.build_path()

    for {app, _} <- Mix.Project.apps_paths(),
        app not in ignored_apps do
      Path.join([build, "lib", Atom.to_string(app), "ebin"])
    end
  end

  defp normalize_main(options) do
    main = options[:main]

    cond do
      is_nil(main) ->
        Keyword.delete(options, :main)

      is_atom(main) ->
        Keyword.put(options, :main, inspect(main))

      is_binary(main) ->
        options
    end
  end

  defp normalize_deps(options) do
    user_deps = Keyword.get(options, :deps, [])

    deps =
      for {app, doc} <- Keyword.merge(get_deps(), user_deps),
          lib_dir = :code.lib_dir(app),
          is_list(lib_dir),
          do: {List.to_string(lib_dir), doc}

    Keyword.put(options, :deps, deps)
  end

  defp get_deps do
    for {key, _} <- Mix.Project.deps_paths(),
        _ = Application.load(key),
        vsn = Application.spec(key, :vsn) do
      {key, "https://hexdocs.pm/#{key}/#{vsn}/"}
    end
  end
end
