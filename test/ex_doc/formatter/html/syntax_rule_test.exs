defmodule ExDoc.Formatter.HTML.SyntaxRuleTest do
  use ExUnit.Case, async: true

  import ExDoc.SyntaxRule
  import Regex, only: [run: 2]

  describe "Elixir: basic parts" do
    test "prefix" do
      assert ["t:"] = run(re(:prefix), "t:term()")
      assert ["c:"] = run(re(:prefix), "c:GenServer.c:init/1")
      refute run(re(:prefix), ":binary.cp()")
    end

    test "modules" do
      refute run(re(:m), "module.fun/1")
      assert "Module" == run_hd(re(:m), "Module.fun/1")
      assert "Module.Some" == run_hd(re(:m), "Module.Some.fun/1")
    end

    test "functions" do
      assert "fun" == run_hd(re(:f), "`fun/1`")
      assert "is_FUN_99?" == run_hd(re(:f), "`is_FUN_99?/1`")

      assert "fun/11" == run_hd(re(:fa), "`fun/11`")
      assert "is_FUN_99?/11" == run_hd(re(:fa), "`is_FUN_99?/11`")
    end

    test "special forms" do
      assert "/" == run_hd(re(:f), "`/2`")
      assert "." == run_hd(re(:f), "`.2`")
      assert "__ENV__" == run_hd(re(:f), "`__ENV__/0`")
    end
  end

  describe "Erlang: basic parts" do
    test "modules" do
      refute run(re(:m, :erlang), "MODULE.FUN/1")
      assert ":module" == run_hd(re(:m, :erlang), ":module.fun/1")
      assert ":module_some" == run_hd(re(:m, :erlang), ":module_some.fun/1")
    end

    test "functions" do
      assert "fun" == run_hd(re(:f, :erlang), "`fun/1`")
      assert "is_FUN_99?!" == run_hd(re(:f, :erlang), "`is_FUN_99?!/1`")

      assert "is_fun_99/123" == run_hd(re(:fa, :erlang), "`is_fun_99/123`")
    end

    test "mfa" do
      assert ":my_erlang_module.is_fun!/123" ==
               run_hd(re(:mfa, :erlang), "`:my_erlang_module.is_fun!/123`")
    end
  end

  describe "links" do
    test "normal links - Elixir" do
      mfa = re_source(:mfa)
      refute run(re({:normal_link, mfa}, :markdown), "[`example`](`Mod.example/1`)")
      assert run(re({:normal_link, mfa}, :markdown), "`Mod.example/1`")
    end

    test "custom links - Elixir" do
      mfa = re_source(:mfa)

      assert "[`example`](`Mod.example/1`)" ==
               run_hd(re({:custom_link, mfa}, :markdown), "[`example`](`Mod.example/1`)")
    end

    test "normal links - Erlang" do
      mfa = re_source(:mfa, :erlang)
      refute run(re({:normal_link, mfa}, :markdown), "[`example`](`:mod.example/1`)")
      assert run_hd(re({:normal_link, mfa}, :markdown), "`:mod.example/1`")
    end

    test "custom links - Erlang" do
      mfa = re_source(:mfa, :erlang)

      assert "[`example`](`:mod.example/1`)" ==
               run_hd(re({:custom_link, mfa}, :markdown), "[`example`](`:mod.example/1`)")

      refute run(re({:custom_link, mfa}, :markdown), "`:mod.example/1`")
    end
  end

  defp run_hd(regex, string) do
    hd(Regex.run(regex, string))
  end
end
