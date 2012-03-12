Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit::Case

  test "generate_markdown generates the markdown file with the documentation" do
    output_dir = File.expand_path("../../output", __FILE__)

    try do
      :file.make_dir(output_dir)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      ExDoc.generate_markdown([file])
      path = output_dir <> "/::CompiledWithDocs.html"
      assert :filelib.is_file(path)
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end

  test "generate_html outputs the correct content" do
    output_dir = File.expand_path("../../output", __FILE__)

    try do
      expected = """
      <p>moduledoc</p>

      <div>
      example/0
      Some example
      </div>
      <div>
      example_1/0
      Another example
      </div>
      """

      :file.make_dir(output_dir)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      ExDoc.generate_markdown([file])
      path = output_dir <> "/::CompiledWithDocs.html"
      { :ok, generated } = :file.read_file(path)
      assert_equal expected, generated
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end
end
