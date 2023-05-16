defmodule ExDoc.Formatter.HTML.SearchItemsTest do
  use ExUnit.Case, async: true
  import TestHelper

  @moduletag :tmp_dir

  test "Elixir module", c do
    modules =
      elixirc(c, ~S'''
      defmodule SearchFoo do
        @moduledoc """
        Hello

        ## Section 1

        Section Content 1.
        """
      end
      ''')

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc"}
    [item] = search_items(modules, config)
    assert item["ref"] == "SearchFoo.html"
    assert item["type"] == "module"
    assert item["title"] == "SearchFoo"
    assert item["doc"] == "Hello\n\n## Section 1\n\nSection Content 1."
  end

  test "module with no docs", c do
    modules =
      elixirc(c, ~S'''
      defmodule SearchFoo do
      end
      ''')

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc"}
    [item] = search_items(modules, config)
    assert item["ref"] == "SearchFoo.html"
    assert item["type"] == "module"
    assert item["title"] == "SearchFoo"
    assert item["doc"] == ""
  end

  @tag :otp_eep48
  test "Erlang module", c do
    [module] =
      erlc(c, :search_foo, """
      %% @doc
      %% Hello <em>world</em>.
      -module(search_foo).
      """)

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc"}
    [item] = search_items([module], config)
    assert item["ref"] == "search_foo.html"
    assert item["type"] == "module"
    assert item["title"] == "search_foo"
    assert item["doc"] == "Hello world ."
  end

  test "function", c do
    modules =
      elixirc(c, ~S'''
      defmodule SearchFoo do
        @doc """
        Hello *world*.
        """
        def foo, do: :ok
      end
      ''')

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc"}
    [%{"type" => "module"}, item] = search_items(modules, config)
    assert item["ref"] == "SearchFoo.html#foo/0"
    assert item["type"] == "function"
    assert item["title"] == "SearchFoo.foo/0"
    assert item["doc"] == "Hello *world*."
  end

  test "callback", c do
    modules =
      elixirc(c, ~S'''
      defmodule SearchFoo do
        @doc """
        Handles _foo_.
        """
        @callback handle_foo() :: :ok
      end
      ''')

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc"}
    [%{"type" => "behaviour"}, item] = search_items(modules, config)
    assert item["ref"] == "SearchFoo.html#c:handle_foo/0"
    assert item["type"] == "callback"
    assert item["title"] == "SearchFoo.handle_foo/0"
    assert item["doc"] == "Handles _foo_."
  end

  test "type", c do
    modules =
      elixirc(c, ~S'''
      defmodule SearchFoo do
        @typedoc """
        The _foo_ type.
        """
        @type foo() :: atom()
      end
      ''')

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc"}
    [%{"type" => "module"}, item] = search_items(modules, config)
    assert item["ref"] == "SearchFoo.html#t:foo/0"
    assert item["type"] == "type"
    assert item["title"] == "SearchFoo.foo/0"
    assert item["doc"] == "The _foo_ type."
  end

  test "extras", c do
    readme_path = "#{c.tmp_dir}/README.md"

    File.write!(readme_path, """
    # Foo

    _Foo_ content.

    ## Section 1 Header

    Section _1_ content.
    """)

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc", extras: [readme_path]}
    [item1, item2] = search_items([], config)

    assert item1["ref"] == "readme.html"
    assert item1["type"] == "extras"
    assert item1["title"] == "Foo"
    assert item1["doc"] == "# Foo\n\n_Foo_ content."

    assert item2["ref"] == "readme.html#section-1-header"
    assert item2["type"] == "extras"
    assert item2["title"] == "Foo - Section 1 Header"
    assert item2["doc"] == "Section _1_ content."
  end

  defp search_items(modules, config) do
    modules = ExDoc.Retriever.docs_from_modules(modules, config)

    ExDoc.Formatter.HTML.run(modules, config)
    [path] = Path.wildcard(Path.join([config.output, "dist", "search_items-*.js"]))
    "searchNodes=" <> json = File.read!(path)
    Jason.decode!(json)
  end
end
