Code.require_file "../test_helper", __FILE__

defmodule ExDocTest do
  use ExUnit::Case

  test "get_docs returns the module name" do
    tmp = File.expand_path("../tmp", __FILE__)
    path = File.expand_path("../fixtures/compiled_with_docs.ex", __FILE__)

    try do
      :file.make_dir(tmp)
      Code.compile_file_to_dir(path, tmp, docs: true)
      Code.prepend_path(tmp)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      assert_match [{ "::CompiledWithDocs", _ }], ExDoc.get_docs([file])
    after:
      :os.cmd('rm -rf #{tmp}')
    end
  end

  test "get_docs returns the moduledoc info" do
    tmp = File.expand_path("../tmp", __FILE__)

    try do
      :file.make_dir(tmp)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      [{ _, {moduledoc, _} }] = ExDoc.get_docs([file])
      assert_match { 1, "moduledoc" }, moduledoc
    after:
      :os.cmd('rm -rf #{tmp}')
    end
  end

  test "get_docs returns nil if there's no moduledoc info" do
    tmp = File.expand_path("../tmp", __FILE__)
    path = File.expand_path("../fixtures/compiled_without_docs.ex", __FILE__)

    try do
      :file.make_dir(tmp)
      Code.compile_file_to_dir(path, tmp, docs: true)

      file = File.expand_path("../tmp/::CompiledWithoutDocs.beam", __FILE__)
      [{ _, {moduledoc, _} }] = ExDoc.get_docs([file])
      assert_match { _, nil }, moduledoc
    after:
      :os.cmd('rm -rf #{tmp}')
    end
  end

  test "get_docs returns the doc info for each module function" do
    tmp = File.expand_path("../tmp", __FILE__)

    try do
      :file.make_dir(tmp)

      file = File.expand_path("../tmp/::CompiledWithDocs.beam", __FILE__)
      [{ _, {_, doc} }] = ExDoc.get_docs([file])
      assert_match [{_, _, _, "Some example"}], doc
    after:
      :os.cmd('rm -rf #{tmp}')
    end
  end

  test "get_docs returns an empty list if there's no docs info" do
    tmp = File.expand_path("../tmp", __FILE__)

    try do
      :file.make_dir(tmp)

      file = File.expand_path("../tmp/::CompiledWithoutDocs.beam", __FILE__)
      [{ _, {_, doc} }] = ExDoc.get_docs([file])
      assert_empty doc
    after:
      :os.cmd('rm -rf #{tmp}')
    end
  end
end
