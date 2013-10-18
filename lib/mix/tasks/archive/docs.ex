defmodule Mix.Tasks.Archive.Docs do
  use Mix.Task

  @shortdoc "Create an archive of HTML documentation for the project"

  @moduledoc """
  Uses ExDoc to generate a package with the static web pages of the documentation.

  This invokes `Mix.Task.Docs` and then packages the resulting documentation.

  This will create the files in a temporary directory.

  Note that all html files, css files and js files in the docs directory will
  get included in the archive.
  """

  @doc false
  def run(_args, config // Mix.project, generator // &ExDoc.generate_docs/3) do
    # Only allowed argument is the output dir, but we force our own output dir.
    { { year, month, day }, { hour, minute, second } } = :calendar.universal_time()
    tmpdir = "tmp-docs-#{year}-#{month}-#{day}-#{hour}-#{minute}-#{second}"
    File.mkdir!(tmpdir)
    try do
      Mix.Tasks.Docs.run_with_cli_opts([output: tmpdir], config, generator)
      tmpdir = "tmp-docs-#{year}-#{month}-#{day}-#{hour}-#{minute}-#{second}"
      zipfile = Mix.Archive.name(config[:name] || config[:app], config[:version] || "dev")
                |> String.replace(%r/\.ez$/, "-docs.zip")
      files = Path.wildcard(Path.join(tmpdir, "**"))
              |> Enum.map(&(Path.relative_to(&1, tmpdir) |> String.to_char_list!()))
      res = :zip.create(zipfile, files, 
                        [{ :compress, :all }, { :cwd, String.to_char_list!(tmpdir) }])
      case res do
        { :error, reason } ->
          raise Mix.Error, message: "Error while creating zip file: #{inspect reason}"
        { :ok, _ } -> :ok
      end
    after
      File.rm_rf!(tmpdir)
    end
  end
end
