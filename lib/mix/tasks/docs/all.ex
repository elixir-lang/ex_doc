defmodule Mix.Tasks.Docs.All do
  use Mix.Task

  @shortdoc "Generate HTML documentation for the project and all dependencies"
  @recursive true
  @moduledoc """
  Uses ExDoc to generate a static web page from the docstrings extracted from
  all of the project's modules and do the same for all dependencies. Effectively
  invokes `mix docs` for the project and every dependency and collects the
  generated documentation in a single place with a nice index file.

  ## Command line options

  * `--output`, `-o` - output directory for the generated docs; each project will
                       have a subdirectory beneath this, default: `"docs"`

  ## Configuration

  See the `docs` task.
  """

  @doc false
  def run(args, _config // Mix.project, generator // &ExDoc.generate_docs/3) do
    Mix.Task.run "deps.check"

    { cli_opts, args, _ } = OptionParser.parse(args, aliases: [o: :output], switches: [output: :string])

    if args != [] do
      raise Mix.Error, message: "Extraneous arguments on the command line"
    end
    
    docroot_rel = cli_opts[:output] || "docs"
    docroot = Path.absname(docroot_rel)
    
    Enum.each(Mix.Deps.loaded, fn dep ->
      Mix.Deps.in_dependency(dep, fn 
        nil -> :ok # Non-elixir dependency
        _   ->
          overrides = [create_assets: false, create_index: false]
          config = Keyword.update(Mix.Project.config, :docs, overrides,
                      &Keyword.merge(&1, overrides))
          Mix.Tasks.Docs.run_internal(config, generator, [output: docroot]) 
      end)
    end)
    
    res = Mix.Tasks.Docs.run_internal(Mix.Project.config, generator, [output: docroot]) 
    log(docroot_rel)
    res
  end

  defp log(docroot) do
    index = Path.join(docroot, "index.html")
    Mix.shell.info "%{green}Docs successfully generated."
    Mix.shell.info "%{green}Open #{index} in your browser to read them."
  end
end

