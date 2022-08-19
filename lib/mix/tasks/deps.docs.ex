defmodule Mix.Tasks.Deps.Docs do
  use Mix.Task

  @shortdoc "Generate documentation for the project dependencies"
  @requirements ["compile"]

  @moduledoc ~S"""
  Uses ExDoc to generate a static web page from the project dependencies documentation.

  See `mix help docs` for more information.
  """

  @switches [
    canonical: :string,
    formatter: :keep,
    language: :string,
    open: :boolean,
    output: :string
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
          raise("expected :name or :app to be found in the project definition in mix.exs")
      )

    project = project <> " (dependencies)"
    version = config[:version] || "dev"
    root_docs_config = Keyword.take(config[:docs] || [], ~w[extra_section language logo]a)

    options =
      config
      |> get_deps_docs()
      |> normalize_apps(config)
      |> Keyword.merge(cli_opts)
      |> Keyword.merge(root_docs_config)

    Mix.shell().info("Generating docs...")

    for formatter <- get_formatters(options) do
      index = generator.(project, version, Keyword.put(options, :formatter, formatter))
      Mix.shell().info([:green, "View #{inspect(formatter)} docs at #{inspect(index)}"])
      if cli_opts[:open], do: browser_open(index)
      index
    end
  end

  defp get_formatters(options) do
    case Keyword.get_values(options, :formatter) do
      [] -> options[:formatters] || ["html", "epub"]
      values -> values
    end
  end

  defp get_deps_docs(options) do
    key = :skip_undefined_reference_warnings_on
    acc = [{key, []}, apps: [], extras: [], source_beam: []]
    build = Mix.Project.build_path()

    for {app, path} <- Mix.Project.deps_paths(),
        app not in Keyword.get(options, :ignore_apps, []),
        dep_docs = dep_docs(app, path),
        reduce: acc do
      acc ->
        extras = normalize_extras_paths(dep_docs[:extras], app)
        compile_path = Path.join([build, "lib", Atom.to_string(app), "ebin"])
        skip_undefined_reference_warnings_on = normalize_paths(dep_docs[key], app)

        acc
        |> Keyword.update!(:apps, &[app | &1])
        |> Keyword.update!(:extras, &(&1 ++ extras))
        |> Keyword.update!(:source_beam, &[compile_path | &1])
        |> Keyword.update!(key, &(&1 ++ skip_undefined_reference_warnings_on))
        |> normalize_groups(dep_docs)
    end
  end

  defp dep_docs(app, path) do
    Mix.Project.in_project(app, path, fn _module ->
      Mix.Project.config()[:docs] || []
    end)
  end

  defp normalize_extras_paths(list, app) do
    Enum.map(list || [], &normalize_extras_path(&1, app))
  end

  defp normalize_extras_path({name, opts}, app) do
    filename = "#{app}-#{opts[:filename] || name}"
    title = "#{opts[:tile] || name} (#{app})"
    path = name |> normalize_path(app) |> String.to_atom()
    {path, Keyword.merge(opts, filename: filename, title: title)}
  end

  defp normalize_extras_path(name, app), do: normalize_extras_path({name, []}, app)

  defp normalize_paths(list, app), do: Enum.map(list || [], &normalize_path(&1, app))

  defp normalize_path(name, app) do
    Path.join(["deps", Atom.to_string(app), to_string(name)])
  end

  defp normalize_groups(acc, dep_docs) do
    group_names = ~w[groups_for_extras groups_for_functions groups_for_modules]a

    Enum.reduce(group_names, acc, fn group_name, acc ->
      group = dep_docs[group_name]

      if is_nil(group) do
        acc
      else
        Keyword.update(acc, group_name, group, &(&1 ++ group))
      end
    end)
  end

  defp normalize_apps(docs, options) do
    docs_options = Keyword.get(options, :docs, [])
    user_deps = docs_options |> Keyword.get(:deps, []) |> Keyword.keys()

    if user_deps == [] do
      docs
    else
      Keyword.replace!(docs, :apps, docs[:apps] ++ user_deps)
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
