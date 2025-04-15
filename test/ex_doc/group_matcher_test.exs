defmodule ExDoc.GroupMatcherTest do
  use ExUnit.Case, async: true
  import ExDoc.GroupMatcher

  describe "group_by" do
    test "group by given data with leftovers" do
      assert group_by([1, 3, 5], [%{key: 1}, %{key: 3}, %{key: 2}], & &1.key) == [
               {1, [%{key: 1}]},
               {3, [%{key: 3}]},
               {2, [%{key: 2}]}
             ]
    end
  end

  describe "module matching" do
    test "by atom names" do
      patterns = [
        Group: [MyApp.SomeModule, :lists]
      ]

      assert match_module(patterns, MyApp.SomeModule, "MyApp.SomeModule", %{}) ==
               :Group

      assert match_module(patterns, :lists, ":lists", %{}) == :Group

      assert match_module(
               patterns,
               MyApp.SomeOtherModule,
               "MyApp.SomeOtherModule",
               %{}
             ) ==
               nil
    end

    test "by string names" do
      patterns = [
        Group: ["MyApp.SomeModule", ":lists"]
      ]

      assert match_module(patterns, MyApp.SomeModule, "MyApp.SomeModule", %{}) ==
               :Group

      assert match_module(patterns, :lists, ":lists", %{}) == :Group

      assert match_module(patterns, MyApp.SomeOtherModule, "MyApp.SomeOtherModule", %{}) ==
               nil
    end

    test "by regular expressions" do
      patterns = [
        Group: ~r/MyApp\..?/
      ]

      assert match_module(patterns, MyApp.SomeModule, "MyApp.SomeModule", %{}) ==
               :Group

      assert match_module(patterns, MyApp.SomeOtherModule, "MyApp.SomeOtherModule", %{}) ==
               :Group

      assert match_module(patterns, MyAppWeb.SomeOtherModule, "MyAppWeb.SomeOtherModule", %{}) ==
               nil
    end
  end

  describe "extras matching" do
    test "by string names" do
      patterns = [
        Group: ["docs/handling/testing.md"]
      ]

      assert match_extra(patterns, {:path, "docs/handling/testing.md"}) == :Group
      refute match_extra(patterns, {:path, "docs/handling/setup.md"})
    end

    test "by regular expressions" do
      patterns = [
        Group: ~r/docs\/handling?/
      ]

      assert match_extra(patterns, {:path, "docs/handling/testing.md"}) == :Group
      assert match_extra(patterns, {:path, "docs/handling/setup.md"}) == :Group
      refute match_extra(patterns, {:path, "docs/introduction.md"})
    end

    test "for extras with a title and a url" do
      patterns = [Group: ~r/elixir/i]

      assert match_extra(patterns, {:url, "Elixir Packages", "https://hex.pm"}) == :Group
      assert match_extra(patterns, {:url, "Website", "https://elixir-lang.org"}) == :Group
      refute match_extra(patterns, {:url, "Example", "https://example.com"})
    end
  end
end
