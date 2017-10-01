defmodule ExDoc.GroupMatcherTest do
  use ExUnit.Case, async: true
  import ExDoc.GroupMatcher

  describe "module matching" do
    test "it can match modules by their atom names" do
      patterns = [
        "Group": [MyApp.SomeModule, :lists]
      ]
      assert match_module(patterns, MyApp.SomeModule, "MyApp.SomeModule") == :"Group"
      assert match_module(patterns, :lists, ":lists") == :"Group"
      assert match_module(patterns, MyApp.SomeOtherModule, "MyApp.SomeOtherModule") == nil
    end

    test "it can match modules by their string names" do
      patterns = [
        "Group": ["MyApp.SomeModule", ":lists"]
      ]
      assert match_module(patterns, MyApp.SomeModule, "MyApp.SomeModule") == :"Group"
      assert match_module(patterns, :lists, ":lists") == :"Group"
      assert match_module(patterns, MyApp.SomeOtherModule, "MyApp.SomeOtherModule") == nil
    end

    test "it can match modules by regular expressions" do
      patterns = [
        "Group": ~r/MyApp\..?/
      ]
      assert match_module(patterns, MyApp.SomeModule, "MyApp.SomeModule") == :"Group"
      assert match_module(patterns, MyApp.SomeOtherModule, "MyApp.SomeOtherModule") == :"Group"
      assert match_module(patterns, MyAppWeb.SomeOtherModule, "MyAppWeb.SomeOtherModule") == nil
    end
  end

  describe "extras matching" do
    test "it can match extra files by their string names" do
      patterns = [
        "Group": ["docs/handling/testing.md"]
      ]
      assert match_extra(patterns, "docs/handling/testing.md") == :"Group"
      assert match_extra(patterns, "docs/handling/setup.md") == nil
    end

    test "it can match extra files by regular expressions" do
      patterns = [
        "Group": ~r/docs\/handling?/
      ]
      assert match_extra(patterns, "docs/handling/testing.md") == :"Group"
      assert match_extra(patterns, "docs/handling/setup.md") == :"Group"
      assert match_extra(patterns, "docs/introduction.md") == nil
    end
  end
end
