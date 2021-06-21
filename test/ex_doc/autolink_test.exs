defmodule ExDoc.AutolinkTest do
  use ExUnit.Case, async: true

  describe "url/2" do
    test "module" do
      assert url!({:module, String}) == "https://hexdocs.pm/elixir/String.html"
      assert url!({:module, :array}) == "https://erlang.org/doc/man/array.html"

      # same app
      assert url!({:module, String}, apps: [:elixir]) == "String.html"
      assert url!({:module, :array}, apps: [:stdlib]) == "array.html"

      # same module
      assert url!({:module, String}, module: String) == "#content"
      assert url!({:module, :array}, module: :array) == "#content"
    end

    test "escaping" do
      assert url!({:function, Kernel.SpecialForms, :%{}, 1}, module: Kernel.SpecialForms) ==
               "#%25%7B%7D/1"

      assert url!({:function, Kernel.SpecialForms, :{}, 1}, module: Kernel.SpecialForms) ==
               "#%7B%7D/1"

      assert url!({:function, Kernel.SpecialForms, :<<>>, 1}, module: Kernel.SpecialForms) ==
               "#%3C%3C%3E%3E/1"
    end
  end

  defp url!(ref, opts \\ []) do
    config = struct!(ExDoc.Autolink, opts)
    {:ok, url} = ExDoc.Autolink.url(ref, inspect(ref), :always, config)
    url
  end
end
