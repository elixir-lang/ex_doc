Code.require_file "../../test_helper", __FILE__

defmodule ExDoc::HTMLFormatterTest do
  use ExUnit::Case

  test "format_docs generate only the module name when there's no more info" do
    output_dir = File.expand_path("../../../output", __FILE__)

    try do
      :file.make_dir(output_dir)

      ExDoc::HTMLFormatter.format_docs({"::XPTOModule", {{1, nil}, []}})
      path = File.expand_path(output_dir <> "/::XPTOModule.html", __FILE__)
      expected = """
      <!DOCTYPE html>
      <html>
        <head>
          <title>::XPTOModule</title>
          <meta charset="utf-8">
          <link rel="stylesheet" href="css/main.css" type="text/css" media="screen" charset="utf-8">
        </head>

        <body>
          <div class="banner">
            <span>Elixir v0.9</span>
            <h1>::XPTOModule</h1>
          </div>
          <div id="bodyContent">
            <div id="content">
              
              
              
            </div>
          </div>
        </body>
      </html>
      """
      assert_equal expected, File.read!(path)
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end

  test "generate_docs outputs the correct content" do
    output_dir = File.expand_path("../../../output", __FILE__)

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

              
              
                <div class="sectiontitle">Macros</div>
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

      file = File.expand_path("../../tmp/::CompiledWithDocs.beam", __FILE__)
      [docs] = ExDoc::Retriever.get_docs([file])
      ExDoc::HTMLFormatter.format_docs(docs)
      path = output_dir <> "/::CompiledWithDocs.html"
      { :ok, generated } = :file.read_file(path)
      assert_equal expected, generated
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end
end
