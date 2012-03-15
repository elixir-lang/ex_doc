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
end
