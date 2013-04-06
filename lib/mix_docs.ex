defexception ConfigError, message: "configuration error"

defmodule Mix.Tasks.Docs do
  use Mix.Task

  @shortdoc "Generate HTML documentation for the project"

  @moduledoc """
  There is a bunch of customizable options available. Put them under your project's :doc key.

  Your project must have :name defined. If you also define :version, it will be
  used. Otherwise, the version will be set to "dev".

    :output       output directory for the generated docs; default: docs
    :formatter    doc formatter to use; default: ExDoc.HTMLFormatter
    :source_root  path to the source code root directory; default: . (current directory)
    :source_url   public URL of the project (optional)
    :main         main module of the project, will be shown on the starting page

  """

  def run(args) do
    if nil?(project = Mix.project[:name]) do
      raise ConfigError, message: "Undefined project name (:name option)"
    end
    if nil?(version = Mix.project[:version]) do
      version = "dev"
    end

    options = Mix.project[:doc_options]
    if is_atom(options[:main]) do
      options = Keyword.update(options, :main, fn(mod) -> Module.to_binary(mod) end)
    end
    if formatter = options[:formatter] do
      options = Keyword.put(options, :formatter, String.split(formatter, "."))
    end

    ExDoc.generate_docs(project, version, options)
  end
end
