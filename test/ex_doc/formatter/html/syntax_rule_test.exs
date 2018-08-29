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
      assert "Module" == run(re(:m), "Module.fun/1") |> hd()
      assert "Module.Some" == run(re(:m), "Module.Some.fun/1") |> hd()
    end

    test "functions" do
      assert "fun" == run(re(:f), "`fun/1`") |> hd()
      assert "is_FUN_99?" == run(re(:f), "`is_FUN_99?/1`") |> hd()

      assert "fun/11" == run(re(:fa), "`fun/11`") |> hd()
      assert "is_FUN_99?/11" == run(re(:fa), "`is_FUN_99?/11`") |> hd()
    end

    test "special forms" do
      assert "/" == run(re(:f), "`/2`") |> hd()
      assert "." == run(re(:f), "`.2`") |> hd()
      assert "__ENV__" == run(re(:f), "`__ENV__/0`") |> hd()
    end
  end

  describe "Erlang: basic parts" do
    test "modules" do
      refute run(re(:m, :erlang), "MODULE.FUN/1")
      assert ":module" == run(re(:m, :erlang), ":module.fun/1") |> hd()
      assert ":module_some" == run(re(:m, :erlang), ":module_some.fun/1") |> hd()
    end

    test "functions" do
      assert "fun" == run(re(:f, :erlang), "`fun/1`") |> hd()
      assert "is_FUN_99?!" == run(re(:f, :erlang), "`is_FUN_99?!/1`") |> hd()

      assert "is_fun_99/123" == run(re(:fa, :erlang), "`is_fun_99/123`") |> hd()
    end

    test "mfa" do
      assert ":my_erlang_module.is_fun!/123" ==
               run(re(:mfa, :erlang), "`:my_erlang_module.is_fun!/123`") |> hd()
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
               run(re({:custom_link, mfa}, :markdown), "[`example`](`Mod.example/1`)") |> hd()
    end

    test "normal links - Erlang" do
      mfa = re_source(:mfa, :erlang)
      refute run(re({:normal_link, mfa}, :markdown), "[`example`](`:mod.example/1`)")
      assert run(re({:normal_link, mfa}, :markdown), "`:mod.example/1`")
    end

    test "custom links - Erlang" do
      mfa = re_source(:mfa, :erlang)

      assert "[`example`](`:mod.example/1`)" ==
               run(re({:custom_link, mfa}, :markdown), "[`example`](`:mod.example/1`)") |> hd()

      refute run(re({:custom_link, mfa}, :markdown), "`:mod.example/1`")
    end
  end
end
