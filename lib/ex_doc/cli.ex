defmodule ExDoc.CLI do
  def run(args) do
    { options, argv } = OptionParser.parse(args, aliases: [o: :output, f: :formatter, p: :project_url])
    path = Enum.first(argv) || "ebin"
    ExDoc.generate_docs(path, options)
  end
end
