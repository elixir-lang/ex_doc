exclude = if ExDoc.Markdown.Pandoc.available?, do: [], else: [pandoc: true]
ExUnit.start(exclude: exclude)

if Enum.empty? Path.wildcard("test/tmp/ebin/*.beam") do
  IO.puts "Compiling fixtures..."
  {result, _error_code} = System.cmd "elixirc", ["test/fixtures", "-o", "test/tmp/ebin"], stderr_to_stdout: true
  IO.puts result
end

Code.prepend_path "test/tmp/ebin"
