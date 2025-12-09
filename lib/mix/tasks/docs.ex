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

    * `--warnings-as-errors` - Exits with non-zero exit code if any warnings are found

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
            favicon: "path/to/favicon.png",
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

    * `:assets` - A map of source => target directories that will be copied as is to
      the output path. It defaults to an empty map.

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
      (`.livemd` extension), Cheatsheets (`.cheatmd` extension), external urls (`:url` option),
      and plain text pages to add to the documentation. You can also specify keyword pairs to
      customize the generated filename, title and source file, and search content of each extra page;
      default: `[]`. Example: `["README.md", "LICENSE", "CONTRIBUTING.md": [filename: "contributing",
      title: "Contributing", source: "CONTRIBUTING.mdx"]]` See the Customizing Extras section for
      more.

    * `:favicon` - Path to a favicon image file for the project. Must be PNG, JPEG or SVG. When
      specified, the image file will be placed in the output "assets" directory, named
      "favicon.EXTENSION". If using SVG, ensure appropriate width, height and viewBox attributes
      are present in order to ensure predictable sizing and cropping.

    * `:filter_modules` - Include only modules that match the given value. The
      value can be a regex, a string (representing a regex), or a two-arity
      function that receives the module and its metadata and returns true if the
      module must be included. If a string or a regex is given, it will be matched
      against the complete module name (which includes the "Elixir." prefix for
      Elixir modules). If a module has `@moduledoc false`, then it is always excluded.

    * `:formatters` - Formatter to use; default: ["html", "epub"], options: "html", "epub".

    * `:footer` - When false, does not render the footer on all pages, except for
      the required "Built with ExDoc" note.

    * `:groups_for_extras`, `:groups_for_modules`, `:groups_for_docs`, and `:default_group_for_doc` -
      See the "Groups" section.

    * `:ignore_apps` - Apps to be ignored when generating documentation in an umbrella project.
      Receives a list of atoms. Example: `[:first_app, :second_app]`.

    * `:language` - Identify the primary language of the documents, its value must be
      a valid [BCP 47](https://tools.ietf.org/html/bcp47) language tag. Default: "en".

    * `:logo` - Path to a logo image file for the project. Must be PNG, JPEG or SVG. When
      specified, the image file will be placed in the output "assets" directory, named
      "logo.EXTENSION". The image will be shown within a 48x48px area. If using SVG, ensure
      appropriate width, height and viewBox attributes are present in order to ensure
      predictable sizing and cropping.

    * `:main` - Main page of the documentation. It may be a module or a
      generated page, like "Plug" or "api-reference". Default: "api-reference".

    * `:markdown_processor` - The markdown processor to use,
      either `module()` or `{module(), keyword()}` to provide configuration options.

    * `:meta` - A keyword list or a map to specify meta tag attributes.

    * `:nest_modules_by_prefix` - See the "Nesting" section.

    * `:output` - Output directory for the generated docs. Default: "doc".
      May be overridden by command line argument.

    * `:redirects` - A map or list of tuples, where the key is the path to redirect from and the
       value is the path to redirect to. The extension is omitted in both cases, i.e `%{"old-readme" => "readme"}`.
       See the "Changing documentation over time" section below for more.

    * `:search` - A list of search engine configurations. See the "Search engines" section.

    * `:skip_undefined_reference_warnings_on` - ExDoc warns when it can't create a `Mod.fun/arity`
      reference in the current project docs (for example, because of a typo). This option controls when to
      skip such warnings. This option can be a list of strings that will be checked for exact matches,
      or a function that takes a *reference* and must return a boolean (`true` means "skip this").
      *References* that are checked against this option (either whether they're in the given
      list or whether they match the given function) are the relative filename, the "ID" of
      the node (like `User.exists?/1`), or the module name. Examples for this option:
        * `["Foo", "Bar.baz/0"]` - skip warnings for `Foo` and `Bar.baz/0`
        * `&String.match?(&1, ~r/Foo/)` - skip warnings for any reference that matches the regex
        * `["pages/deprecations.md"]` - skip warnings for any reference in the
          `pages/deprecations.md` file

    * `:skip_code_autolink_to` - Similar to `:skip_undefined_reference_warnings_on`, this option
      controls which terms will be skipped by ExDoc when building documentation.
      Useful for example if you want to highlight private modules or functions without warnings.
      This option can be a function from a term to a boolean (e.g.: `&String.match?(&1, ~r/PrivateModule/)`)
      or a list of terms (e.g.:`["PrivateModule", "PrivateModule.func/1"]`);
      default is nothing to be skipped.

    * `:source_beam` - Path to the beam directory; default: mix's compile path.

    * `:source_url` - The source URL fallback if `:source_url` is not given at the project
      configuration. See the following sections.

    * `:source_ref` - The branch/commit/tag used for source link inference.
      Default: "main".

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

      For self-hosted GitLab/GitHub:

      ```text
      https://mydomain.org/user_or_team/repo_name/blob/main/%{path}#L%{line}
      ```

      For self-hosted Bitbucket:

      ```text
      https://mydomain.org/user_or_team/repo_name/src/main/%{path}#cl-%{line}
      ```

      If a function, then it must be a function that takes two arguments, path and line,
      where path is either an relative path from the cwd, or an absolute path. The function
      must return the full URI as it should be placed in the documentation.

  ### Using `:source_url` and `:source_ref` together

  A common setup for a project or library is to set both `:source_url` and `:source_ref`. Setting
  both of them will allow ExDoc to link to specific version of the code for a function or module
  that matches the version of the docs. So if the docs have been generated for version 1.0.5 then
  clicking on the source link in the docs will take the browser to the source code for the 1.0.5
  version of the code instead of only the primary ref (for example, `main`).

  A example setup looks like:

      @version "0.30.10"
      def project do
        [
          ...
          version: @version,
          source_url: "https://github.com/USER/PROJECT",
          docs: &docs/0
        ]
      end

      def docs do
        [
          ...
          source_ref: "v#{@version}"
        ]
      end

  If you use `source_ref: "v#{@version}"` then when publishing a new version of your package you
  should run `git tag vVERSION` and push the tag. This way, ExDoc will generate links to the
  specific version the docs were generated for.

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

  External extras from a URL can also be grouped:

      groups_for_extras: [
        "Elixir": ~r"https://elixir-lang.org/",
        "Erlang": ~r"https://www.erlang.org/"
      ]

  Similar can be done for modules:

      groups_for_modules: [
        "Data types": [Atom, Regex, URI],
        "Collections": [Enum, MapSet, Stream]
      ]

  A regex or the string name of the module is also supported.

  ### Grouping functions, types, and callbacks

  Types, functions, and callbacks inside a module can also be organized in groups.

  #### Group metadata

  By default, ExDoc respects the `:group` metadata field to determine in which
  group an element belongs:

      @doc group: "Queries"
      def get_by(schema, fields)

  The function above will be automatically listed under the "Queries" section in
  the sidebar. The benefit of using `:group` is that it can also be used by tools
  such as IEx during autocompletion. These groups are then displayed in the sidebar.

  It is also possible to tell ExDoc to either enrich the group metadata or lookup a
  different field via the `:default_group_for_doc` configuration. The default is:

      default_group_for_doc: fn metadata -> metadata[:group] end

  The `metadata` received contains all of the documentation metadata, such as `:group`,
  but also `:module`, `:name`, `:arity` and `:kind` to help identify which entity is
  currently being processed. For example, projects like Nx have a custom function that
  converts "Queries" into "Function: Queries":

      default_group_for_doc: fn metadata ->
        if group = metadata[:group] do
          "Functions: #{group}"
        end
      end

  Finally, you can also use the `:groups_for_docs` which works similarly as the
  one for modules/extra pages.

  `:groups_for_docs` is a keyword list of group titles and filtering functions
  that receive the documentation metadata and must return a boolean.
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

  A function can belong to a single group only. The first group that matches
  will be the one used. In case no group is found in `:groups_for_docs`,
  the `:default_group_for_doc` callback is invoked. If it returns nil, it
  then falls back to the appropriate "Functions", "Types" or "Callbacks"
  section respectively.

  #### Group descriptions

  It is possible to display a description for each group under its respective section
  in a module's page. This helps to better explain what is the intended usage of each
  group elements instead of describing everything in the displayed `@moduledoc`.

  Descriptions can be provided as `@moduledoc` metadata. Groups without descriptions are
  also supported to define group ordering.

      @moduledoc groups: [
        "Main API",
        %{title: "Helpers", description: "Functions shared with other modules."}
      ]

  Descriptions can also be given in the `:default_group_for_doc` configuration:

      default_group_for_doc: fn metadata ->
        case metadata[:group] do
          :main_api -> "Main API"
          :helpers -> [title: "Helpers", description: "Functions shared with other modules."]
          _ -> nil
        end
      end

  Keyword lists or maps are supported in either case.

  When using `:groups_for_docs`, if all the elements for a given group are matched then the
  `:default_group_for_doc` is never invoked and ExDoc will not know about the description.
  In that case, the description should be provided in the `@moduledoc` `:groups` metadata.

  Whenever using the `:group` key, the groups will be ordered alphabetically.
  If you also want control over the group order, you can also use the `:groups_for_docs`
  which works similarly as the one for modules/extra pages.

  #### Group ordering

  Groups in the sidebar and main page body are ordered according to the following
  rules:

    * First, groups defined as `@moduledoc groups: [...]` in the given order.
    * Then groups defined as keys in the `:groups_for_docs` configuration.
    * Then default groups: Types, Callbacks and Functions.
    * Finally, other groups returned by `:default_group_for_doc` by alphabetical order.

  ## Search engines

  ExDoc allows custom search engines via the `:search` key. Each search engine
  is a map with the following keys:

    * `:name` - The display name of the search engine (required)
    * `:help` - A help text describing what the search engine does (required)
    * `:url` - The optional search URL template, usually ending with `q=`
    * `:packages` - An optional list of packages (or package-versions) to search on
      https://hexdocs.pm. For example: `[:plug, :phoenix, ecto: "3.0.0", ecto_sql: "3.0.0"]`.
      If no version is specified, it uses the package latest

  If none of `:url` or `:packages` are given, ExDoc will use its default search engine
  powered by Lunr.

  When multiple search engines are configured, a dropdown selector will appear next to
  the search bar allowing users to choose which engine to use. For example:

      search: [
        %{name: "FooBar", help: "Search on FooBar", url: "https://example.com/?q="},
        %{name: "Local", help: "In-browser search"}
      ]

  If only one search engine is configured, the dropdown selector will be hidden.
  If no search engine is configured, only the built-in Lunr's is shown.

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

  ## Changing documentation over time

  As your project grows, your documentation may very likely change, even structurally.
  There are a few important things to consider in this regard:

    * Links to your *extras* will break if you change or move file names.
    * Links to your *modules, and mix tasks* will change if you change their name.
    * Links to *functions* are actually links to modules with anchor links.
      If you change the function name, the link does not break but will leave users
      at the top of the module's documentation.

  Because these docs are static files, the behavior of a missing page will depend on where they are hosted.
  In particular, [hexdocs.pm](https://hexdocs.pm) will show a 404 page.

  You can improve the developer experience on everything but function names changing
  by using the `redirects` configuration. For example, if you changed the module `MyApp.MyModule`
  to `MyApp.My.Module` and the extra `get-started.md` to `quickstart.md`, you can
  setup the following redirects:

      redirects: %{
        "MyApp.MyModule" => "MyApp.My.Module",
        "get-started" => "quickstart"
      }

  ## Customizing Extras

  There are two sources for extras, filenames and urls.

  For filenames, the allowed configuration is:

    * `:title` - The title of the extra page. If not provided, the title will be inferred from the filename.
    * `:filename` - The name of the generated file. If not provided, the filename will be inferred from
       the source file.
    * `:source` - The source file of the extra page. This is useful if you want to customize the filename or
       title but keep the source file unchanged.
    * `:search_data` - A list of terms to be indexed for autocomplete and search. If not provided, the content
       of the extra page will be indexed for search. See the section below for more.

  For urls:

    * `:title` - The title of the extra page. If not provided, the title will be inferred from the extra name.
    * `:url` - The external url to link to from the sidebar.

  ### Customizing search data

  It is possible to fully customize the way a given extra is indexed, both in autocomplete and in search.
  In most cases, this makes sense for _generated_ documentation. If `search_data` is provided, it completely
  overrides the built in logic for indexing your document based on the headers and content of the document.
  The following fields can be provided in a list of maps for `search_data`.

    * `:anchor` - The anchor link for the search result. Use `""` to point to the top of the page.
    * `:title` - The title of the result.
    * `:type` - The type of the search result, such as "module", "function" or "section".
    * `:body` - The main content or body of the search result, _as markdown_. Used in search, not autocomplete.

  ## Umbrella project

  ExDoc can be used in an umbrella project and generates a single documentation
  for all child apps. You can use the `:ignore_apps` configuration to exclude
  certain projects in the umbrella from documentation.

  Generating documentation per each child app can be achieved by running:

      mix cmd mix docs

  See `mix help cmd` for more information.

  ## Meta-tags configuration

  It is also possible to configure some of ExDoc behaviour using meta tags.
  These meta tags can be inserted using `before_closing_head_tag`.

    * `exdoc:autocomplete` - when set to "off", it disables autocompletion.

    * `exdoc:autocomplete-limit` - Set to an integer to configure how many results
      appear in the autocomplete dropdown. Defaults to 10.
  """

  @switches [
    canonical: :string,
    formatter: :keep,
    language: :string,
    open: :boolean,
    output: :string,
    proglang: :string,
    warnings_as_errors: :boolean
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

    Code.prepend_path(options[:source_beam])

    for path <- Keyword.get_values(options, :paths),
        path <- Path.wildcard(path) do
      Code.prepend_path(path)
    end

    Mix.shell().info("Generating docs...")

    results =
      for formatter <- options[:formatters] do
        index = generator.(project, version, Keyword.put(options, :formatter, formatter))
        Mix.shell().info([:green, "View #{inspect(formatter)} docs at #{inspect(index)}"])

        if cli_opts[:open] do
          browser_open(index)
        end

        if options[:warnings_as_errors] == true and ExDoc.Utils.unset_warned() do
          {:error, %{reason: :warnings_as_errors, formatter: formatter}}
        else
          {:ok, index}
        end
      end

    error_results = Enum.filter(results, &(elem(&1, 0) == :error))

    if error_results == [] do
      Enum.map(results, fn {:ok, value} -> value end)
    else
      formatters = Enum.map(error_results, &elem(&1, 1).formatter)

      format_message =
        case formatters do
          [formatter] -> "#{formatter} format"
          _ -> "#{Enum.join(formatters, ", ")} formats"
        end

      message =
        "Documents have been generated, but generation for #{format_message} failed due to warnings while using the --warnings-as-errors option."

      message_formatted = IO.ANSI.format([:red, message, :reset])
      IO.puts(:stderr, message_formatted)
      exit({:shutdown, 1})
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
