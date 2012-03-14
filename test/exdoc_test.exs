Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit::Case

  test "generate_html generates the markdown file with the documentation" do
    output_dir = File.expand_path("../../output", __FILE__)

    try do
      :file.make_dir(output_dir)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      ExDoc.generate_html([file])
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
      <!DOCTYPE html>
      <html>
        <head>
          <title>::CompiledWithDocs</title>

          <meta charset="utf-8">
        </head>

        <body>
          <h1>::CompiledWithDocs</h1>
      <div id="moduledoc">
      <p>moduledoc</p>

      <h2>Example</h2>

      <pre><code>CompiledWithDocs.example
      </code></pre>

      </div>

      <div id="example_0">
      example/0
      <p>Some example</p>

      </div>
      <div id="example_1_0">
      example_1/0
      <p>Another example</p>

      </div>

        </body>
      </html>
      """

      :file.make_dir(output_dir)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      ExDoc.generate_html([file])
      path = output_dir <> "/::CompiledWithDocs.html"
      { :ok, generated } = :file.read_file(path)
      assert_equal expected, generated
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end
end
