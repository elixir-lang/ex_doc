Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit::Case

  test "generate_markdown generates the markdown file with the documentation" do
    output_dir = File.expand_path("../../output", __FILE__)

    try do
      :file.make_dir(output_dir)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      ExDoc.generate_markdown([file])
      path = output_dir <> "/::CompiledWithDocs.md"
      assert :filelib.is_file(path)
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end

  test "generate_markdown outputs the correct content" do
    output_dir = File.expand_path("../../output", __FILE__)

    expected = """
    moduledoc

    <div>
    example/0
    Some example
    </div>
    <div>
    example_1/0
    Another example
    </div>
    """

    try do
      :file.make_dir(output_dir)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      ExDoc.generate_markdown([file])
      path = output_dir <> "/::CompiledWithDocs.md"
      assert_match { :ok, expected }, :file.read_file(path)
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end
end
