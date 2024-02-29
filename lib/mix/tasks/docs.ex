defmodule Mix.Tasks.Docs do
  use Mix.Task

  @shortdoc "Generate documentation for the project"
  @requirements ["compile"]

  @moduledoc ~S"""
  Uses ExDoc to generate a static web page from the project documentation.

  ## Command line options

    * `--canonical`, `-n` - Indicate the preferred URL with
      `rel="canonical"` link element, defaults to no canonical path

    * `--formatter`, `-f` - Which formatters to use, `html` or
      `epub`. This option can be given more than once. By default,
      both `html` and `epub` are generated.

    * `--language` - Specifies the language to annotate the
      EPUB output in valid [BCP 47](https://tools.ietf.org/html/bcp47)

    * `--open` - open browser window pointed to the documentation

    * `--output`, `-o` - Output directory for the generated
      docs, default: `"doc"`

    * `--proglang` - Chooses the main programming language: `elixir`
      or `erlang`

  The command line options have higher precedence than the options
  specified in your `mix.exs` file below.

  ## Configuration

  ExDoc will automatically pull in information from your project,
  like the application and version. However, you may want to set
  `:name`, `:source_url` and `:homepage_url` to have a nicer output
  from ExDoc, for example:

      def project do
        [
          app: :my_app,
          version: "0.1.0-dev",
          deps: deps(),

          # Docs
          name: "My App",
          source_url: "https://github.com/USER/PROJECT",
          homepage_url: "http://YOUR_PROJECT_HOMEPAGE",
          docs: [
            main: "MyApp", # The main page in the docs
            logo: "path/to/logo.png",
            extras: ["README.md"]
          ]
        ]
      end

  ExDoc also allows configuration specific to the documentation to
  be set. The following options should be put under the `:docs` key
  in your project's main configuration. The `:docs` options should
  be a keyword list or a function returning a keyword list that will
  be lazily executed.

    * `:annotations_for_docs` - a function that receives metadata and returns a list
      of annotations to be added to the signature. The metadata received will also
      contain `:module`, `:name`, `:arity` and `:kind` to help identify which entity is
      currently being processed.

    * `:api_reference` - Whether to generate `api-reference.html`; default: `true`.
      If this is set to false, `:main` must also be set.

    * `:assets` - Path to a directory that will be copied as is to the "assets"
      directory in the output path. Its entries may be referenced in your docs
      under "assets/ASSET.EXTENSION"; defaults to no assets directory.

    * `:authors` - List of authors for the generated docs or epub.

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

    * `:before_closing_footer_tag` - a function that takes as argument an atom specifying
      the formatter being used (`:html`) and returns a literal HTML string
      to be included just before the closing footer tag (`</footer>`).
      This option only has effect on the html formatter.
      Useful if you want to inject an extra footer into the documentation.

    * `:canonical` - String that defines the preferred URL with the rel="canonical"
      element; defaults to no canonical path.

    * `:cover` - Path to the epub cover image (only PNG or JPEG accepted)
      The image size should be around 1600x2400. When specified, the cover will be placed under
      the "assets" directory in the output path under the name "cover" and the
      appropriate extension. This option has no effect when using the "html" formatter.

    * `:deps` - A keyword list application names and their documentation URL.
      ExDoc will by default include all dependencies and assume they are hosted on
      HexDocs. This can be overridden by your own values. Example: `[plug: "https://myserver/plug/"]`

    * `:extra_section` - String that defines the section title of the additional
      Markdown and plain text pages; default: "PAGES". Example: "GUIDES"

    * `:extras` - List of paths to additional Markdown (`.md` extension), Live Markdown
      (`.livemd` extension), Cheatsheets (`.cheatmd` extension) and plain text pages to
      add to the documentation. You can also specify keyword pairs to customize the
      generated filename, title and source file of each extra page; default: `[]`. Example:
      `["README.md", "LICENSE", "CONTRIBUTING.md": [filename: "contributing", title: "Contributing", source: "CONTRIBUTING.mdx"]]`

    * `:filter_modules` - Include only modules that match the given value. The
      value can be a regex, a string (representing a regex), or a two-arity
      function that receives the module and its metadata and returns true if the
      module must be included. If a string or a regex is given, it will be matched
      against the complete module name (which includes the "Elixir." prefix for
      Elixir modules). If a module has `@moduledoc false`, then it is always excluded.

    * `:formatters` - Formatter to use; default: ["html", "epub"], options: "html", "epub".

    * `:groups_for_extras`, `:groups_for_modules`, `:groups_for_docs` - See the "Groups" section

    * `:ignore_apps` - Apps to be ignored when generating documentation in an umbrella project.
      Receives a list of atoms. Example: `[:first_app, :second_app]`.

    * `:javascript_config_path` - Path of an additional JavaScript file to be included on all pages
      to provide up-to-date data for features like the version dropdown - See the "Additional
      JavaScript config" section. Example: `"../versions.js"`

    * `:language` - Identify the primary language of the documents, its value must be
      a valid [BCP 47](https://tools.ietf.org/html/bcp47) language tag; default: "en"

    * `:logo` - Path to a logo image file for the project. Must be PNG, JPEG or SVG. When
      specified, the image file will be placed in the output "assets" directory, named
      "logo.EXTENSION". The image will be shown within a 48x48px area. If using SVG, ensure
      appropriate width, height and viewBox attributes are present in order to ensure
      predictable sizing and cropping.

    * `:main` - Main page of the documentation. It may be a module or a
      generated page, like "Plug" or "api-reference"; default: "api-reference".

    * `:markdown_processor` - The markdown processor to use,
      either `module()` or `{module(), keyword()}` to provide configuration options;

    * `:nest_modules_by_prefix` - See the "Nesting" section

    * `:output` - Output directory for the generated docs; default: "doc".
      May be overridden by command line argument.

    * `:skip_undefined_reference_warnings_on` - ExDoc warns when it can't create a `Mod.fun/arity`
      reference in the current project docs e.g. because of a typo. This list controls where to
      skip the warnings, for a given module/function/callback/type (e.g.: `["Foo", "Bar.baz/0"]`)
      or on a given file (e.g.: `["pages/deprecations.md"]`); default: `[]`.

    * `:skip_code_autolink_to` - Similar to `:skip_undefined_reference_warnings_on`, this option
      controls which terms will be skipped by ExDoc when building documentation.
      Useful for example if you want to highlight private modules or functions
      without warnings (e.g.: `["PrivateModule", "PrivateModule.func/1"]`);
      default: `[]`.

    * `:source_beam` - Path to the beam directory; default: mix's compile path.

    * `:source_ref` - The branch/commit/tag used for source link inference;
      default: "main".

    * `:source_url_pattern` - Public URL of the project for source links. This is derived
      automatically from the project's `:source_url` and `:source_ref` when using one of
      the supported public hosting services (currently GitHub, GitLab, or Bitbucket). If
      you are using one of those services with their default public hostname, you do not
      need to set this configuration.

      However, if using a different solution, or self-hosting, you will need to set this
      configuration variable to a pattern for source code links. The value must be a string or
      a function.

      If a string, then it should be the full URI to use for links with the following
      variables available for interpolation:

        * `%{path}`: the path of a file in the repo
        * `%{line}`: the line number in the file

      For GitLab/GitHub:

      ```text
      https://mydomain.org/user_or_team/repo_name/blob/main/%{path}#L%{line}
      ```

      For Bitbucket:

      ```text
      https://mydomain.org/user_or_team/repo_name/src/main/%{path}#cl-%{line}
      ```

      If a function, then it must be a function that takes two arguments, path and line,
      where path is either an relative path from the cwd, or an absolute path. The function
      must return the full URI as it should be placed in the documentation.

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
        "guides/advanced/bat.md"
      ]

  You can have those grouped as follows:

      groups_for_extras: [
        "Introduction": Path.wildcard("guides/introduction/*.md"),
        "Advanced": Path.wildcard("guides/advanced/*.md")
      ]

  Or via a regex:

      groups_for_extras: [
        "Introduction": ~r"/introduction/",
        "Advanced": ~r"/advanced/"
      ]

  Similar can be done for modules:

      groups_for_modules: [
        "Data types": [Atom, Regex, URI],
        "Collections": [Enum, MapSet, Stream]
      ]

  A regex or the string name of the module is also supported.

  ### Grouping functions and callbacks

  Functions and callbacks inside a module can also be organized in groups.
  This is done via the `:groups_for_docs` configuration which is a
  keyword list of group titles and filtering functions that receive the
  documentation metadata of functions as argument. The metadata received will also
  contain `:module`, `:name`, `:arity` and `:kind` to help identify which entity is
  currently being processed.

  For example, imagine that you have an API client library with a large surface
  area for all the API endpoints you need to support. It would be helpful to
  group the functions with similar responsibilities together. In this case in
  your module you might have:

      defmodule APIClient do
        @doc section: :auth
        def refresh_token(params \\ [])

        @doc subject: :object
        def update_status(id, new_status)

        @doc permission: :grant
        def grant_privilege(resource, privilege)
      end

  And then in the configuration you can group these with:

      groups_for_docs: [
        Authentication: & &1[:section] == :auth,
        Resource: & &1[:subject] == :object,
        Admin: & &1[:permission] in [:grant, :write]
      ]

  A function can belong to a single group only. If multiple group filters match,
  the first will take precedence. Functions and callbacks that don't have a
  custom group will be listed under the default "Functions" and "Callbacks"
  group respectively.

  ## Additional JavaScript config

  Since version `0.20.0` ExDoc includes a way to enrich the documentation
  with new information without having to re-generate it, through a JavaScript
  file that can be shared across documentation for multiple versions of the
  package. If `:javascript_config_path` is set when building the documentation,
  this script will be referenced in each page's `<head>` using a `<script>` tag.
  The script should define data in global JavaScript variables that will be
  interpreted by `ex_doc` when viewing the documentation.

  Currently supported variables:

  ### `versionNodes`

  This global JavaScript variable should be providing an array of objects that
  define all versions of this Mix package which should appear in the package
  versions dropdown in the documentation sidebar. The versions dropdown allows
  for switching between package versions' documentation.

  Example:

  ```javascript
  var versionNodes = [
    {
      version: "v0.0.0", // version number or name (required)
      url: "https://hexdocs.pm/ex_doc/0.19.3/" // documentation URL (required)
    }
  ]
  ```

  ## Nesting

  ExDoc also allows module names in the sidebar to appear nested under a given
  prefix. The `:nest_modules_by_prefix` expects a list of module names, such as
  `[Foo.Bar, Bar.Baz]`. In this case, a module named `Foo.Bar.Baz` will appear
  nested within `Foo.Bar` and only the name `Baz` will be shown in the sidebar.
  Note the `Foo.Bar` module itself is not affected.

  This option is mainly intended to improve the display of long module names in
  the sidebar, particularly when they are too long for the sidebar or when many
  modules share a long prefix. If you mean to group modules logically or call
  attention to them in the docs, you should probably use `:groups_for_modules`
  (which can be used in conjunction with `:nest_modules_by_prefix`).

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
    open: :boolean,
    output: :string,
    proglang: :string
  ]

  @aliases [
    f: :formatter,
    n: :canonical,
    o: :output
  ]

  @doc false
  def run(args, config \\ Mix.Project.config(), generator \\ &ExDoc.generate_docs/3) do
    {:ok, _} = Application.ensure_all_started(:ex_doc)

    unless Code.ensure_loaded?(ExDoc.Config) do
      Mix.raise(
        "Could not load ExDoc configuration. Please make sure you are running the " <>
          "docs task in the same Mix environment it is listed in your deps"
      )
    end

    {cli_opts, args, _} = OptionParser.parse(args, aliases: @aliases, switches: @switches)

    if args != [] do
      Mix.raise("Extraneous arguments on the command line")
    end

    project =
      to_string(
        config[:name] || config[:app] ||
          Mix.raise("expected :name or :app to be found in the project definition in mix.exs")
      )

    version = config[:version] || "dev"

    cli_opts =
      Keyword.update(cli_opts, :proglang, :elixir, fn proglang ->
        if proglang not in ~w(erlang elixir) do
          Mix.raise("--proglang must be elixir or erlang")
        end

        String.to_atom(proglang)
      end)

    options =
      config
      |> get_docs_opts()
      |> Keyword.merge(cli_opts)
      # accepted at root level config
      |> normalize_source_url(config)
      # accepted at root level config
      |> normalize_homepage_url(config)
      |> normalize_source_beam(config)
      |> normalize_apps(config)
      |> normalize_main()
      |> normalize_deps()
      |> normalize_formatters()
      |> put_package(config)

    Mix.shell().info("Generating docs...")

    for formatter <- options[:formatters] do
      index = generator.(project, version, Keyword.put(options, :formatter, formatter))
      Mix.shell().info([:green, "View #{inspect(formatter)} docs at #{inspect(index)}"])

      if cli_opts[:open] do
        browser_open(index)
      end

      index
    end
  end

  defp normalize_formatters(options) do
    formatters =
      case Keyword.get_values(options, :formatter) do
        [] -> options[:formatters] || ["html", "epub"]
        values -> values
      end

    Keyword.put(options, :formatters, formatters)
  end

  defp get_docs_opts(config) do
    docs = config[:docs]

    cond do
      is_function(docs, 0) -> docs.()
      is_nil(docs) -> []
      true -> docs
    end
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

  defp normalize_apps(options, config) do
    if Mix.Project.umbrella?(config) do
      ignore = Keyword.get(options, :ignore_apps, [])

      apps =
        for {app, _} <- Mix.Project.apps_paths(), app not in ignore do
          app
        end

      Keyword.put(options, :apps, Enum.sort(apps))
    else
      Keyword.put(options, :apps, List.wrap(config[:app]))
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
          do: {app, doc}

    Keyword.put(options, :deps, deps)
  end

  defp get_deps do
    for {key, _} <- Mix.Project.deps_paths(),
        _ = Application.load(key),
        vsn = Application.spec(key, :vsn) do
      {key, "https://hexdocs.pm/#{key}/#{vsn}/"}
    end
  end

  defp put_package(options, config) do
    if package = config[:package] do
      Keyword.put(options, :package, package[:name] || config[:app])
    else
      options
    end
  end

  defp browser_open(path) do
    {cmd, args, options} =
      case :os.type() do
        {:win32, _} ->
          dirname = Path.dirname(path)
          basename = Path.basename(path)
          {"cmd", ["/c", "start", basename], [cd: dirname]}

        {:unix, :darwin} ->
          {"open", [path], []}

        {:unix, _} ->
          {"xdg-open", [path], []}
      end

    System.cmd(cmd, args, options)
  end
end
