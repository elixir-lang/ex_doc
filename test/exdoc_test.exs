Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit::Case, sync: true

  test "generate_docs generates the html file with the documentation" do
    output_dir = File.expand_path("../../output", __FILE__)

    try do
      :file.make_dir(output_dir)

      ExDoc.generate_docs File.expand_path("../tmp", __FILE__)
      path = output_dir <> "/::CompiledWithDocs.html"
      assert :filelib.is_file(path)
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end

  test "generate_docs accepts relative directories" do
    output_dir = File.expand_path("../../output", __FILE__)

    try do
      :file.make_dir(output_dir)

      ExDoc.generate_docs "test/tmp"
      path = output_dir <> "/::CompiledWithDocs.html"
      assert :filelib.is_file(path)
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end

  test "generate_docs generates the index html file with all modules" do
    output_dir = File.expand_path("../../output", __FILE__)

    try do
      :file.make_dir(output_dir)

      expected = """
      <!DOCTYPE html>
      <html>
        <head>
          <title>Modules</title>
          <meta charset="utf-8">
          <link rel="stylesheet" href="css/main.css" type="text/css" media="screen" charset="utf-8">
        </head>

        <body>
          <ul>
            <li><a href='::CompiledWithDocs.html' target='_blank'>::CompiledWithDocs</a>
      <ul>
      <li><a href='::CompiledWithDocs.html#example/0' target='_blank'>example/0</a></li>
      <li><a href='::CompiledWithDocs.html#example_1/0' target='_blank'>example_1/0</a></li>
      </ul>
      </li>
      <li><a href='::CompiledWithoutDocs.html' target='_blank'>::CompiledWithoutDocs</a></li>

          </ul>
        </body>
      </html>
      """

      ExDoc.generate_docs File.expand_path("../tmp", __FILE__)
      path = output_dir <> "/index.html"
      generated = File.read!(path)
      assert_equal expected, generated
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end
end
