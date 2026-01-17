defmodule ExDoc.ExtrasTest do
  use ExUnit.Case, async: true

  @moduletag :tmp_dir

  alias ExDoc.Extras

  defp config(opts) do
    config = %ExDoc.Config{
      project: "Elixir",
      version: "1.0.0",
      extras: Keyword.get(opts, :extras, []),
      groups_for_extras: Keyword.get(opts, :groups_for_extras, []),
      source_url_pattern: Keyword.get(opts, :source_url_pattern, fn _path, _line -> nil end)
    }

    struct!(config, opts)
  end

  describe "build/1" do
    test "builds extras from markdown files", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/readme.md", """
      # README

      This is a readme file.
      """)

      config = config(extras: ["#{tmp_dir}/readme.md"])
      [extra] = Extras.build(config)

      assert %Extras.Page{} = extra
      assert extra.id == "readme"
      assert extra.title == "README"
      assert extra.source_doc =~ "This is a readme file"
      assert extra.type == :extra
    end

    test "builds extras from .livemd files", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/notebook.livemd", """
      # Livebook

      A livebook file.
      """)

      config = config(extras: ["#{tmp_dir}/notebook.livemd"])
      [extra] = Extras.build(config)

      assert %Extras.Page{} = extra
      assert extra.type == :livemd
      assert extra.title == "Livebook"
    end

    test "builds extras from .cheatmd files", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/cheat.cheatmd", """
      # Cheatsheet

      Quick reference.
      """)

      config = config(extras: ["#{tmp_dir}/cheat.cheatmd"])
      [extra] = Extras.build(config)

      assert %Extras.Page{} = extra
      assert extra.type == :cheatmd
      assert extra.title == "Cheatsheet"
    end

    test "builds extras from .txt files", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/plain.txt", """
      This is plain text.
      No markdown here.
      """)

      config = config(extras: ["#{tmp_dir}/plain.txt"])
      [extra] = Extras.build(config)

      assert %Extras.Page{} = extra
      assert extra.type == :extra
      assert extra.title == "plain"
      assert extra.source_doc =~ "This is plain text"
    end

    test "builds extras from files with no extension", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/LICENSE", """
      License text here.
      """)

      config = config(extras: ["#{tmp_dir}/LICENSE"])
      [extra] = Extras.build(config)

      assert %Extras.Page{} = extra
      assert extra.type == :extra
      assert extra.title == "LICENSE"
    end

    test "builds URL extras" do
      config = config(extras: ["Elixir": [url: "https://elixir-lang.org"]])
      [extra] = Extras.build(config)

      assert %Extras.URL{} = extra
      assert extra.id == "elixir"
      assert extra.title == "Elixir"
      assert extra.url == "https://elixir-lang.org"
    end

    test "builds mixed page and URL extras", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/readme.md", "# README")

      config =
        config(extras: ["#{tmp_dir}/readme.md", "Elixir": [url: "https://elixir-lang.org"]])

      [page, url] = Extras.build(config)

      assert %Extras.Page{} = page
      assert page.id == "readme"

      assert %Extras.URL{} = url
      assert url.id == "elixir"
    end

    test "disambiguates extras with the same name", %{tmp_dir: tmp_dir} do
      File.mkdir_p!("#{tmp_dir}/foo")
      File.write!("#{tmp_dir}/foo/README.md", "# README foo")

      File.mkdir_p!("#{tmp_dir}/bar")
      File.write!("#{tmp_dir}/bar/README.md", "# README bar")

      config = config(extras: ["#{tmp_dir}/foo/README.md", "#{tmp_dir}/bar/README.md"])
      [extra1, extra2] = Extras.build(config)

      assert %Extras.Page{} = extra1
      assert %Extras.Page{} = extra2

      assert extra1.id == "readme-1"
      assert extra2.id == "readme-2"
    end

    test "extracts title from h1 header", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/page.md", """
      # My Great Page

      Content here.
      """)

      config = config(extras: ["#{tmp_dir}/page.md"])
      [extra] = Extras.build(config)

      assert extra.title == "My Great Page"
      assert extra.title_doc == ["My Great Page"]
    end

    test "uses filename as title when no h1 exists", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/NoHeader.md", """
      Just some content without a header.
      """)

      config = config(extras: ["#{tmp_dir}/NoHeader.md"])
      [extra] = Extras.build(config)

      assert extra.title == "NoHeader"
    end

    test "supports custom title via options", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/readme.md", "# Default Title")

      config = config(extras: [{"#{tmp_dir}/readme.md", [title: "Custom Title"]}])
      [extra] = Extras.build(config)

      assert extra.title == "Custom Title"
    end

    test "supports custom filename via options", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/readme.md", "# README")

      config = config(extras: [{"#{tmp_dir}/readme.md", [filename: "custom-name"]}])
      [extra] = Extras.build(config)

      assert extra.id == "custom-name"
    end

    test "supports custom source via options", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/readme.md", "# README")

      source_url_pattern = fn path, _line ->
        "https://example.com/#{path}"
      end

      config =
        config(
          extras: [{"#{tmp_dir}/readme.md", [source: "custom/path.md"]}],
          source_url_pattern: source_url_pattern
        )

      [extra] = Extras.build(config)

      assert extra.source_path == "custom/path.md"
      assert extra.source_url == "https://example.com/custom/path.md"
    end

    test "adds IDs to h2 and h3 headers", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/page.md", """
      # Main Title

      ## Section One

      ### Subsection

      ## Section Two
      """)

      config = config(extras: ["#{tmp_dir}/page.md"])
      [extra] = Extras.build(config)

      assert [{:h2, attrs1, _, _}, {:h3, attrs2, _, _}, {:h2, attrs3, _, _}] = extra.doc

      assert Keyword.get(attrs1, :id) == "section-one"
      assert Keyword.get(attrs2, :id) == "subsection"
      assert Keyword.get(attrs3, :id) == "section-two"
    end

    test "supports groups for extras", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/guide.md", "# Guide")
      File.write!("#{tmp_dir}/reference.md", "# Reference")

      config =
        config(
          extras: ["#{tmp_dir}/guide.md", "#{tmp_dir}/reference.md"],
          groups_for_extras: [
            Guides: ~r/guide/,
            Reference: ~r/reference/
          ]
        )

      [guide, reference] = Extras.build(config)

      assert guide.group == :Guides
      assert reference.group == :Reference
    end

    test "groups URL extras" do
      config =
        config(
          extras: ["Elixir": [url: "https://elixir-lang.org"]],
          groups_for_extras: [External: ~r/elixir/i]
        )

      [extra] = Extras.build(config)

      assert %Extras.URL{} = extra
      assert extra.group == :External
    end

    test "sorts extras by group index", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/aaa.md", "# AAA")
      File.write!("#{tmp_dir}/bbb.md", "# BBB")
      File.write!("#{tmp_dir}/ccc.md", "# CCC")

      config =
        config(
          extras: ["#{tmp_dir}/ccc.md", "#{tmp_dir}/aaa.md", "#{tmp_dir}/bbb.md"],
          groups_for_extras: [
            Second: ~r/bbb/,
            First: ~r/aaa/,
            Third: ~r/ccc/
          ]
        )

      extras = Extras.build(config)

      # Sorted by the order groups appear in groups_for_extras
      assert length(extras) == 3
      [first, second, third] = extras

      # bbb.md matches Second (index 0), aaa.md matches First (index 1), ccc.md matches Third (index 2)
      assert first.group == :Second
      assert first.title == "BBB"

      assert second.group == :First
      assert second.title == "AAA"

      assert third.group == :Third
      assert third.title == "CCC"
    end

    test "validates search_data option", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/page.md", "# Page")

      search_data = [
        %{anchor: "test", body: "body", title: "title", type: "extra"}
      ]

      config = config(extras: [{"#{tmp_dir}/page.md", [search_data: search_data]}])
      [extra] = Extras.build(config)

      assert extra.search_data == search_data
    end

    test "raises on invalid search_data", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/page.md", "# Page")

      assert_raise ArgumentError, ~r/expected search data to be a list/, fn ->
        config = config(extras: [{"#{tmp_dir}/page.md", [search_data: "invalid"]}])
        Extras.build(config)
      end
    end

    test "raises on invalid file extension", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/file.pdf", "PDF content")

      assert_raise ArgumentError, ~r/file extension not recognized/, fn ->
        config = config(extras: ["#{tmp_dir}/file.pdf"])
        Extras.build(config)
      end
    end

    test "raises when title option is not a string", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/page.md", "# Page")

      assert_raise ArgumentError, ~r/extra field :title must be a string/, fn ->
        config = config(extras: [{"#{tmp_dir}/page.md", [title: 123]}])
        Extras.build(config)
      end
    end

    test "raises when filename option is not a string", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/page.md", "# Page")

      assert_raise ArgumentError, ~r/extra field :filename must be a string/, fn ->
        config = config(extras: [{"#{tmp_dir}/page.md", [filename: :atom]}])
        Extras.build(config)
      end
    end

    test "handles extras with keyword list options", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/page.md", "# Page")

      config = config(extras: [{"#{tmp_dir}/page.md", title: "Custom"}])
      [extra] = Extras.build(config)

      assert extra.title == "Custom"
    end
  end
end
