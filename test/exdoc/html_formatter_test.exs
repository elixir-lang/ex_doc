Code.require_file "../../test_helper", __FILE__

defmodule ExDoc.HTMLFormatterTest do
  use ExUnit.Case, sync: true

  def setup_all do
    :file.make_dir(output_dir)
  end

  def teardown_all do
    :os.cmd('rm -rf #{output_dir}')
  end

  defp output_dir do
    File.expand_path "output"
  end

  test "format_docs generate only the module name when there's no more info" do
    ExDoc.HTMLFormatter.format_docs({"XPTOModule", {{1, nil}, []}})

    expected = """
    <!DOCTYPE html>
    <html>
      <head>
        <title>XPTOModule</title>
        <meta charset="utf-8">
        <link rel="stylesheet" href="css/main.css" type="text/css" media="screen" charset="utf-8">
      </head>

      <body>
        <div class="banner">
          <span>Elixir v0.9</span>
          <h1>XPTOModule</h1>
        </div>
        <div id="bodyContent">
          <div id="content">
            
            
            
          </div>
        </div>
      </body>
    </html>
    """
    assert_equal expected, File.read!("#{output_dir}/XPTOModule.html")
  end

  test "generate_docs outputs the correct content" do
    input_path = File.expand_path "test/tmp"
    file = "#{input_path}/CompiledWithDocs.beam"

    [docs] = ExDoc.Retriever.get_docs [file], input_path
    ExDoc.HTMLFormatter.format_docs(docs)
    { :ok, generated } = :file.read_file("#{output_dir}/CompiledWithDocs.html")

    expected = """
    <!DOCTYPE html>
    <html>
      <head>
        <title>CompiledWithDocs</title>
        <meta charset="utf-8">
        <link rel="stylesheet" href="css/main.css" type="text/css" media="screen" charset="utf-8">
      </head>

      <body>
        <div class="banner">
          <span>Elixir v0.9</span>
          <h1>CompiledWithDocs</h1>
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
              <div class="function"><div class="function-title" id="example/0">
    <b>example/0</b>
    </div>
    <div class="description">
    <p>Some example</p>
    </div>
    </div>
    <div class="function"><div class="function-title" id="example_without_docs/0">
    <b>example_without_docs/0</b>
    </div>
    <div class="description">
    </div>
    </div>

            
            
              <div class="sectiontitle">Macros</div>
              <div class="function"><div class="function-title" id="example_1/0">
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
    assert_equal expected, generated
  end
end
