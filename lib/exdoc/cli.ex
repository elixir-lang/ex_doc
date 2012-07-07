defmodule ExDoc.CLI do
  def run(args) do
    { options, argv } = OptionParser.Simple.parse(args, [o: :output, f: :formatter, p: :project_url])
    path = Enum.first(argv) || "ebin"
    ExDoc.generate_docs(path, options)
  end
end
