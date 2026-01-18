defmodule ExDoc.ExtrasTest do
  use ExUnit.Case, async: true

  @moduletag :tmp_dir

  alias ExDoc.Extras

  defp config(opts \\ []) do
    %ExDoc.Config{
      groups_for_extras: Keyword.get(opts, :groups_for_extras, []),
      source_url_pattern: Keyword.get(opts, :source_url_pattern, fn _path, _line -> nil end)
    }
    |> struct!(opts)
  end

  describe "build/2" do
    test "builds extras from markdown files", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/readme.md", """
      # README

      This is a readme file.
      """)

      extras = ["#{tmp_dir}/readme.md"]
      config = config()
      [extra] = Extras.build(extras, config)

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

      extras = ["#{tmp_dir}/notebook.livemd"]
      config = config()
      [extra] = Extras.build(extras, config)

      assert %Extras.Page{} = extra
      assert extra.type == :livemd
      assert extra.title == "Livebook"
    end

    test "builds extras from .cheatmd files", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/cheat.cheatmd", """
      # Cheatsheet

      Quick reference.
      """)

      extras = ["#{tmp_dir}/cheat.cheatmd"]
      config = config()
      [extra] = Extras.build(extras, config)

      assert %Extras.Page{} = extra
      assert extra.type == :cheatmd
      assert extra.title == "Cheatsheet"
    end

    test "builds extras from .txt files", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/plain.txt", """
      This is plain text.
      No markdown here.
      """)

      extras = ["#{tmp_dir}/plain.txt"]
      config = config()
      [extra] = Extras.build(extras, config)

      assert %Extras.Page{} = extra
      assert extra.type == :extra
      assert extra.title == "plain"
      assert extra.source_doc =~ "This is plain text"
    end

    test "builds extras from files with no extension", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/LICENSE", """
      License text here.
      """)

      extras = ["#{tmp_dir}/LICENSE"]
      config = config()
      [extra] = Extras.build(extras, config)

      assert %Extras.Page{} = extra
      assert extra.type == :extra
      assert extra.title == "LICENSE"
    end

    test "builds URL extras" do
      extras = ["Elixir": [url: "https://elixir-lang.org"]]
      config = config()
      [extra] = Extras.build(extras, config)

      assert %Extras.URL{} = extra
      assert extra.id == "elixir"
      assert extra.title == "Elixir"
      assert extra.url == "https://elixir-lang.org"
    end

    test "builds mixed page and URL extras", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/readme.md", "# README")

      extras = ["#{tmp_dir}/readme.md", "Elixir": [url: "https://elixir-lang.org"]]
      config = config()
      [page, url] = Extras.build(extras, config)

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

      extras = ["#{tmp_dir}/foo/README.md", "#{tmp_dir}/bar/README.md"]
      config = config()
      [extra1, extra2] = Extras.build(extras, config)

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

      extras = ["#{tmp_dir}/page.md"]
      config = config()
      [extra] = Extras.build(extras, config)

      assert extra.title == "My Great Page"
      assert extra.title_doc == ["My Great Page"]
    end

    test "uses filename as title when no h1 exists", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/NoHeader.md", """
      Just some content without a header.
      """)

      extras = ["#{tmp_dir}/NoHeader.md"]
      config = config()
      [extra] = Extras.build(extras, config)

      assert extra.title == "NoHeader"
    end

    test "supports custom title via options", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/readme.md", "# Default Title")

      extras = [{"#{tmp_dir}/readme.md", [title: "Custom Title"]}]
      config = config()
      [extra] = Extras.build(extras, config)

      assert extra.title == "Custom Title"
    end

    test "supports custom filename via options", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/readme.md", "# README")

      extras = [{"#{tmp_dir}/readme.md", [filename: "custom-name"]}]
      config = config()
      [extra] = Extras.build(extras, config)

      assert extra.id == "custom-name"
    end

    test "supports custom source via options", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/readme.md", "# README")

      source_url_pattern = fn path, _line ->
        "https://example.com/#{path}"
      end

      extras = [{"#{tmp_dir}/readme.md", [source: "custom/path.md"]}]

      config =
        config(source_url_pattern: source_url_pattern)

      [extra] = Extras.build(extras, config)

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

      extras = ["#{tmp_dir}/page.md"]
      config = config()
      [extra] = Extras.build(extras, config)

      assert [{:h2, attrs1, _, _}, {:h3, attrs2, _, _}, {:h2, attrs3, _, _}] = extra.doc

      assert Keyword.get(attrs1, :id) == "section-one"
      assert Keyword.get(attrs2, :id) == "subsection"
      assert Keyword.get(attrs3, :id) == "section-two"
    end

    test "supports groups for extras", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/guide.md", "# Guide")
      File.write!("#{tmp_dir}/reference.md", "# Reference")

      extras = ["#{tmp_dir}/guide.md", "#{tmp_dir}/reference.md"]

      config =
        config(
          groups_for_extras: [
            Guides: ~r/guide/,
            Reference: ~r/reference/
          ]
        )

      [guide, reference] = Extras.build(extras, config)

      assert guide.group == :Guides
      assert reference.group == :Reference
    end

    test "groups URL extras" do
      extras = ["Elixir": [url: "https://elixir-lang.org"]]

      config =
        config(groups_for_extras: [External: ~r/elixir/i])

      [extra] = Extras.build(extras, config)

      assert %Extras.URL{} = extra
      assert extra.group == :External
    end

    test "sorts extras by group index", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/aaa.md", "# AAA")
      File.write!("#{tmp_dir}/bbb.md", "# BBB")
      File.write!("#{tmp_dir}/ccc.md", "# CCC")

      extras_input = ["#{tmp_dir}/ccc.md", "#{tmp_dir}/aaa.md", "#{tmp_dir}/bbb.md"]

      config =
        config(
          groups_for_extras: [
            Second: ~r/bbb/,
            First: ~r/aaa/,
            Third: ~r/ccc/
          ]
        )

      extras = Extras.build(extras_input, config)

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

      extras = [{"#{tmp_dir}/page.md", [search_data: search_data]}]
      config = config()
      [extra] = Extras.build(extras, config)

      assert extra.search_data == search_data
    end

    test "raises on invalid search_data", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/page.md", "# Page")

      assert_raise ArgumentError, ~r/expected search data to be a list/, fn ->
        extras = [{"#{tmp_dir}/page.md", [search_data: "invalid"]}]
        config = config()
        Extras.build(extras, config)
      end
    end

    test "raises on invalid file extension", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/file.pdf", "PDF content")

      assert_raise ArgumentError, ~r/file extension not recognized/, fn ->
        extras = ["#{tmp_dir}/file.pdf"]
        config = config()
        Extras.build(extras, config)
      end
    end

    test "raises when title option is not a string", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/page.md", "# Page")

      assert_raise ArgumentError, ~r/extra field :title must be a string/, fn ->
        extras = [{"#{tmp_dir}/page.md", [title: 123]}]
        config = config()
        Extras.build(extras, config)
      end
    end

    test "raises when filename option is not a string", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/page.md", "# Page")

      assert_raise ArgumentError, ~r/extra field :filename must be a string/, fn ->
        extras = [{"#{tmp_dir}/page.md", [filename: :atom]}]
        config = config()
        Extras.build(extras, config)
      end
    end

    test "handles extras with keyword list options", %{tmp_dir: tmp_dir} do
      File.write!("#{tmp_dir}/page.md", "# Page")

      extras = [{"#{tmp_dir}/page.md", title: "Custom"}]
      config = config()
      [extra] = Extras.build(extras, config)

      assert extra.title == "Custom"
    end
  end
end
