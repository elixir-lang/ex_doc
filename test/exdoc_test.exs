Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit::Case, sync: true

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

  test "generate_docs generates the index html file with all modules" do
    output_dir = File.expand_path("../../output", __FILE__)

    try do
      :file.make_dir(output_dir)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      file_2 = File.expand_path("../tmp/::CompiledWithoutDocs.beam", __FILE__)
      ExDoc.generate_docs([file, file_2])
      path = output_dir <> "/index.html"
      generated = File.read!(path)
      assert_equal "::CompiledWithDocs, ::CompiledWithoutDocs", generated
    after:
      :os.cmd('rm -rf #{output_dir}')
    end
  end
end
