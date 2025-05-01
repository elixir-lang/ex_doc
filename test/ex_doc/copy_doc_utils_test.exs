defmodule ExDoc.CopyDocUtilsTest do
  use ExUnit.Case, async: true

  alias ExDoc.Config
  alias ExDoc.CopyDocUtils

  import ExUnit.CaptureIO
  import TestHelper

  @moduletag :tmp_dir

  test "copy_doc_info" do
    assert {CopyDocUtilsTest, :func, 2} == CopyDocUtils.copy_doc_info(%{delegate_to: {CopyDocUtilsTest, :func, 2}, copy: true})
    assert nil == CopyDocUtils.copy_doc_info(%{delegate_to: {CopyDocUtilsTest, :func, 2}, copy: false})
    assert {CopyDocUtilsTest, :func, 2} == CopyDocUtils.copy_doc_info(%{copy: {CopyDocUtilsTest, :func, 2}})
    assert capture_io(:stderr, fn -> assert nil == CopyDocUtils.copy_doc_info(%{copy: true}) end) =~ "no `delegate_to` specified"
  end

  @project "Elixir"
  @version "1"
  test "extract_doc",c  do
    elixirc(c, ~S"""
      defmodule Orig do
        @doc "Some doc `local_link1/1`"
        def local_link1(_str) do
          :ok
        end
      end
      defmodule Orig2 do
        def local_link1(_str) do
          :ok
        end
      end
      """)
    new_doc = CopyDocUtils.extract_doc(
      {Orig, :local_link1, 1},
      Config.build(@project, @version, copy_doc_decorator: fn doc, {m, f, a} ->
        assert m == "Orig."
        assert f == "local_link1"
        assert a == 1
        doc
      end))

      assert new_doc == ExDoc.Markdown.to_ast("Some doc `Orig.local_link1/1`")

    config =Config.build(@project, @version, copy_doc_decorator: fn doc, {_m, _f, _a} -> doc end)
    assert CopyDocUtils.extract_doc({Orig2, :local_link1, 1}, config) == nil
    assert CopyDocUtils.extract_doc({Orig2, :local_link2, 1}, config) == nil
    assert CopyDocUtils.extract_doc({NonExisting, :local_link2, 1}, config) == nil
  end

  test "rewrite_doc", c do
    elixirc(c, ~S"""
      defmodule Orig do
      def local_link1(_str) do
        :ok
      end
    end
    """)

    mfa = {Orig, :local_link1, 1}
    doc = "The `local_link1/1` gets rewritten"
    assert CopyDocUtils.rewrite_doc(doc, mfa) == "The `Orig.local_link1/1` gets rewritten"
  end

  test "rewrite_ref", c do
    elixirc(c, ~S"""
      defmodule Orig do
      def local_link1(_str) do
        :ok
      end
    end
    """)

    mfa = {Orig, :local_link1, 1}
    assert CopyDocUtils.rewrite_ref("local_link1/1", mfa) == "Orig.local_link1/1"
  end

  test "build_new_ref", c do
    # This test is a cut down version of the copy_doc_test app
    elixirc(c, ~S"""
    defmodule Imported do
      def imported_link(_str), do: :ok
    end
    defmodule Orig do
      import Imported
      defmodule Orig2 do
      end
      @type special_values :: :none | :all | :some
      @callback callback_link1(String.t()) :: :ok
      @spec local_link1(String.t()) :: :ok
      def local_link1(_str) do
        :ok
      end
      @spec local_link2(String.t()) :: :ok
      def local_link2(some_string)
      def local_link2(str) do
        imported_link(str)
      end
      def local_link3(_str) do
        :ok
      end
      def local_link5(_str) do
        :ok
      end
    end
    """)
    mfa = {Orig, :local_link1, 1}
    # code requiring rewrite
    # * `local_link2/1` (should become `Orig.local_link2/1`)
    # * `c:callback_link1/1` (callback should become `c:Orig.callback_link1/1`)
    # * `local_link3/1` (local function in Orig, but not in Delegate, should become `Orig.local_link3/1`)
    # * `t:special_values/0` (typespec should become `t:Orig.special_values/0`)
    assert CopyDocUtils.rewrite_ref("local_link2/1", mfa) == "Orig.local_link2/1"
    assert CopyDocUtils.rewrite_ref("c:callback_link1/1", mfa) == "c:Orig.callback_link1/1"
    assert CopyDocUtils.rewrite_ref("local_link3/1", mfa) == "Orig.local_link3/1"
    assert CopyDocUtils.rewrite_ref("t:special_values/0", mfa) == "t:Orig.special_values/0"

    # code not requiring rewrite
    # * `:local_link1` (just an atom, not even a link)
    # * `local_link4/1` (local function in Delegate, but not in Orig)
    # * `Orig.local_link2(_str)` (just some code, not even a link)
    # * `Kernel.and/2` (function in other modules)
    # * `and/2` (Kernel modules with the implicit module name)
    # * `imported_link/1` (an imported function, but it requires a full qualifier, not even a link)
    # * `Imported.imported_link/1` (now with full qualifier it beomes a link, but no need to rewrite)
    # * `m:Orig2` (not fully qualified, not even a link)
    # * `m:Orig.Orig2` (now fully qualified and should be a link, but does not need rewriting)
    assert CopyDocUtils.rewrite_ref(":local_link1", mfa) == ":local_link1"
    assert CopyDocUtils.rewrite_ref("local_link4/1", mfa) == "local_link4/1"
    assert CopyDocUtils.rewrite_ref("Orig.local_link2(_str)", mfa) == "Orig.local_link2(_str)"
    assert CopyDocUtils.rewrite_ref("Kernel.and/2", mfa) == "Kernel.and/2"
    assert CopyDocUtils.rewrite_ref("and/2", mfa) == "and/2"
    assert CopyDocUtils.rewrite_ref("imported_link/1", mfa) == "imported_link/1"
    assert CopyDocUtils.rewrite_ref("Imported.imported_link/1", mfa) == "Imported.imported_link/1"
    assert CopyDocUtils.rewrite_ref("m:Orig2", mfa) == "m:Orig2"
    assert CopyDocUtils.rewrite_ref("m:Orig.Orig2", mfa) == "m:Orig.Orig2"
  end

  test "is_module_ref?", c do
    elixirc(c, ~S"""
      defmodule TestModule do
        @callback callback_func() :: :ok
        @type type_func :: :ok
        def normal_func() do
          private_func()
          :ok
        end
        defp private_func(), do: :ok
      end
    """)

    assert CopyDocUtils.is_module_ref?(TestModule, "normal_func", 0) == true
    assert CopyDocUtils.is_module_ref?(TestModule, "private_func", 0) == false
    assert CopyDocUtils.is_module_ref?(TestModule, "callback_func", 0) == true
    assert CopyDocUtils.is_module_ref?(TestModule, "callback_func2", 0) == false
    assert CopyDocUtils.is_module_ref?(TestModule, "type_func", 0) == true
  end

  test "remove_leading_elixir" do
    assert Atom.to_string(__MODULE__) == "Elixir.ExDoc.CopyDocUtilsTest"
    assert CopyDocUtils.remove_leading_elixir(__MODULE__) == "ExDoc.CopyDocUtilsTest."
  end
end
