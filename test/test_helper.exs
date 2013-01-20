ExUnit.start []

if Enum.empty? Path.wildcard("test/tmp/*.beam") do
  IO.puts "Compiling fixtures..."
  IO.puts System.cmd "elixirc test/fixtures -o test/tmp"
end

Code.prepend_path "test/tmp"