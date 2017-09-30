defmodule ExDoc.GroupMatcherTest do
  use ExUnit.Case, async: true
  import ExDoc.GroupMatcher

  describe "module matching" do
    test "it can match modules by their atom names" do
      patterns = [
        "Group": [MyApp.SomeModule, :lists]
      ]
      assert "Group" == match_module(patterns, MyApp.SomeModule, "MyApp.SomeModule")
      assert "Group" == match_module(patterns, :lists, ":lists")
      assert nil == match_module(patterns, MyApp.SomeOtherModule, "MyApp.SomeOtherModule")
    end

    test "it can match modules by their string names" do
      patterns = [
        "Group": ["MyApp.SomeModule", ":lists"]
      ]
      assert "Group" == match_module(patterns, MyApp.SomeModule, "MyApp.SomeModule")
      assert "Group" == match_module(patterns, :lists, ":lists")
      assert nil == match_module(patterns, MyApp.SomeOtherModule, "MyApp.SomeOtherModule")
    end

    test "it can match modules by regular expressions" do
      patterns = [
        "Group": ~r/MyApp\..?/
      ]
      assert "Group" == match_module(patterns, MyApp.SomeModule, "MyApp.SomeModule")
      assert "Group" == match_module(patterns, MyApp.SomeOtherModule, "MyApp.SomeOtherModule")
      assert nil == match_module(patterns, MyAppWeb.SomeOtherModule, "MyAppWeb.SomeOtherModule")
    end
  end

  describe "extras matching" do
    test "it can match extra files by their string names" do
      patterns = [
        "Group": ["docs/handling/testing.md"]
      ]
      assert "Group" == match_extra(patterns, "docs/handling/testing.md")
      assert nil == match_extra(patterns, "docs/handling/setup.md")
    end

    test "it can match extra files by regular expressions" do
      patterns = [
        "Group": ~r/docs\/handling?/
      ]
      assert "Group" == match_extra(patterns, "docs/handling/testing.md")
      assert "Group" == match_extra(patterns, "docs/handling/setup.md")
      assert nil == match_extra(patterns, "docs/introduction.md")
    end
  end
end
