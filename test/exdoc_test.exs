Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit::Case

  test "generate_docs generates the html file with the documentation" do
    output_dir = File.expand_path("../../output", __FILE__)

    try do
      :file.make_dir(output_dir)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      ExDoc.generate_docs([file])
      path = output_dir <> "/::CompiledWithDocs.html"
      assert :filelib.is_file(path)
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end

  test "generate_docs outputs the correct content" do
    output_dir = File.expand_path("../../output", __FILE__)

    try do
      expected = """
      <!DOCTYPE html>
      <html>
        <head>
          <title>::CompiledWithDocs</title>
          <meta charset="utf-8">
          <link rel="stylesheet" href="css/main.css" type="text/css" media="screen" charset="utf-8">
        </head>

        <body>
          <div class="banner">
            <span>Elixir v0.9</span>
            <h1>::CompiledWithDocs</h1>
          </div>
          <div id="bodyContent">
            <div id="content">
              <div id="moduledoc" class="description">
                <p>moduledoc</p>

      <h2>Example</h2>

      <pre><code>CompiledWithDocs.example
      </code></pre>

              </div>
              <div class="sectiontitle">Functions</div>
              <div class="function"><div class="function-title" id="example_0">
      <b>example/0</b>
      </div>
      <div class="description">
      <p>Some example</p>

      </div>
      </div>
      <div class="function"><div class="function-title" id="example_1_0">
      <b>example_1/0</b>
      </div>
      <div class="description">
      <p>Another example</p>

      </div>
      </div>

            </div>
          </div>
        </body>
      </html>
      """

      :file.make_dir(output_dir)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      ExDoc.generate_docs([file])
      path = output_dir <> "/::CompiledWithDocs.html"
      { :ok, generated } = :file.read_file(path)
      assert_equal expected, generated
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end
end
