Mix.start()
Mix.shell(Mix.Shell.Process)

defmodule Mix.Tasks.Archive.DocsTest do
#  use ExUnit.Case, async: true
#
#  defrecord ZipFile, Record.extract(:zip_file, from_lib: "stdlib/include/zip.hrl")
#
#  def run(args, opts) do
#    Mix.Tasks.Archive.Docs.run(args, opts)
#  end
#
#  test "inflects values from app and version" do
#    assert run([], [app: :ex_doc, version: "0.1.0"]) ==
#           "ex_doc-0.1.0-docs.zip"
#  end
#
#  test "contains index.html" do
#    filename = run([], [app: :ex_doc, version: "0.1.0"])
#    case :zip.list_dir(String.to_char_list!(filename)) do
#      { :error, reason } -> flunk("#{reason} in :zip.list_dir")
#      { :ok, entries }   ->
#        IO.inspect(entries)
#        assert Enum.any?(entries, &match?(ZipFile[name: 'index.html'], &1))
#    end
#  end
end
