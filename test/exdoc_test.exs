Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit.Case, sync: true

  test "generate_docs generates the html file with the documentation" do
    output_dir = File.expand_path "output"

    try do
      ExDoc.generate_docs File.expand_path("test/tmp")
      path = output_dir <> "/CompiledWithDocs.html"
      assert :filelib.is_file("#{output_dir}/CompiledWithDocs.html")
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end

  test "generate_docs accepts relative directories" do
    output_dir = File.expand_path "output"

    try do
      ExDoc.generate_docs "test/tmp"
      assert :filelib.is_file("#{output_dir}/CompiledWithDocs.html")
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end

  test "generate_docs generates the panel index html file with all modules" do
    output_dir = File.expand_path "output"

    try do
      expected = """
      <!DOCTYPE html>
      <html>
        <head>
          <title>Modules</title>
          <meta charset="utf-8">
          <link rel="stylesheet" href="../css/reset.css" type="text/css" media="screen" charset="utf-8">
          <link rel="stylesheet" href="../css/panel.css" type="text/css" media="screen" charset="utf-8">
        </head>

        <body>
          <div class="panel panel-tree" id="panel">
            <div class="header">
              <div>
                <label for="search" id="search-label" style="display: none">Search</label>
                <table>
                  <tr>
                    <td>
                      <input type="search" placeholder="Search" autosave="searchdoc" results="10" id="search" autocomplete="off"/>
                    </td>
                  </tr>
                </table>
              </div>
            </div>

            <div class="tree">
              <ul>
                <li class='level_0 closed'>
      <div class='content'>
      <a href='../CompiledWithDocs.html' target='docwin'>CompiledWithDocs</a>
      <div class='icon'></div>
      </div>
      </li>
      <li class='level_1 closed'>
      <div class='content'>
      <a href='../CompiledWithDocs.html#example/0' target='docwin'>example/0</a>
      <div class='icon'></div>
      </div>
      </li>
      <li class='level_1 closed'>
      <div class='content'>
      <a href='../CompiledWithDocs.html#example_1/0' target='docwin'>example_1/0</a>
      <div class='icon'></div>
      </div>
      </li>
      <li class='level_1 closed'>
      <div class='content'>
      <a href='../CompiledWithDocs.html#example_without_docs/0' target='docwin'>example_without_docs/0</a>
      <div class='icon'></div>
      </div>
      </li>
      <li class='level_0 closed'>
      <div class='content'>
      <a href='../CompiledWithoutDocs.html' target='docwin'>CompiledWithoutDocs</a>
      <div class='icon'></div>
      </div>
      </li>
      <li class='level_0 closed'>
      <div class='content'>
      <a href='../ExDocTest.Nested.html' target='docwin'>ExDocTest.Nested</a>
      <div class='icon'></div>
      </div>
      </li>
      <li class='level_1 closed'>
      <div class='content'>
      <a href='../ExDocTest.Nested.html#example/2' target='docwin'>example/2</a>
      <div class='icon'></div>
      </div>
      </li>

              </ul>
            </div>
          </div>
        </body>
      </html>
      """

      ExDoc.generate_docs File.expand_path("test/tmp")
      generated = File.read!("#{output_dir}/panel/index.html")
      assert_equal expected, generated
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end
end
