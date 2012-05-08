defmodule ExDoc.CLI do
  def run(args) do
    { options, argv } = OptionParser.Simple.parse(args, [o: :output, f: :formatter])

    if length(argv) == 0 do
      path = "ebin"
    else:
      [path, _] = argv
    end

    ExDoc.generate_docs(path, options)
  end
end
