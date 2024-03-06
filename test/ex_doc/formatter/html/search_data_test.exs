defmodule ExDoc.Formatter.HTML.SearchDataTest do
  use ExUnit.Case, async: true
  import TestHelper

  @moduletag :tmp_dir

  test "Elixir module", c do
    modules =
      elixirc(c, ~S'''
      defmodule SearchFoo do
        @moduledoc """
        Hello.

        ## Section 1

        Section Content 1.

        #### Section 1a

        Section Content 1a.
        """
      end
      ''')

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc"}
    data = search_data(modules, config)
    assert data["content_type"] == "text/markdown"
    assert data["producer"]["name"] == "ex_doc"
    [item1, item2] = data["items"]

    assert item1["ref"] == "SearchFoo.html"
    assert item1["type"] == "module"
    assert item1["title"] == "SearchFoo"
    assert item1["doc"] == "Hello."

    assert item2["ref"] == "SearchFoo.html#module-section-1"
    assert item2["type"] == "module"
    assert item2["title"] == "Section 1 - SearchFoo"

    assert item2["doc"] == """
           Section Content 1.

           #### Section 1a

           Section Content 1a.\
           """
  end

  test "Mix task", c do
    modules =
      elixirc(c, ~S'''
      defmodule Mix.Tasks.SearchItemTest do
        @moduledoc """
        Test task.

        ## Section 1

            $ mix search_item_test
        """
      end
      ''')

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc"}
    [item1, item2] = search_data(modules, config)["items"]

    assert item1["ref"] == "Mix.Tasks.SearchItemTest.html"
    assert item1["type"] == "task"
    assert item1["title"] == "mix search_item_test"
    assert item1["doc"] == "Test task."

    assert item2["ref"] == "Mix.Tasks.SearchItemTest.html#module-section-1"
    assert item2["type"] == "task"
    assert item2["title"] == "Section 1 - mix search_item_test"
    assert item2["doc"] == "$ mix search_item_test"
  end

  test "module with no docs", c do
    modules =
      elixirc(c, ~S'''
      defmodule SearchFoo do
      end
      ''')

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc"}
    [item] = search_data(modules, config)["items"]
    assert item["ref"] == "SearchFoo.html"
    assert item["type"] == "module"
    assert item["title"] == "SearchFoo"
    assert item["doc"] == ""
  end

  test "escaping", c do
    modules =
      elixirc(c, ~S'''
      defmodule SearchFoo do
        @moduledoc ~S"""
        #{}
        """
      end
      ''')

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc"}
    [item] = search_data(modules, config)["items"]

    assert item["doc"] == ~S"#{}"
  end

  @tag :otp_eep48
  test "Erlang module", c do
    [module] =
      erlc(c, :search_foo, """
      %% @doc
      %% Hello <em>world</em>.
      -module(search_foo).
      """)

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc", proglang: :erlang}
    data = search_data([module], config)
    assert data["content_type"] == "text/plain"
    [item] = data["items"]
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
    [%{"type" => "module"}, item] = search_data(modules, config)["items"]
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

        ## Section

        Section content.
        """
        @callback handle_foo() :: :ok
      end
      ''')

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc"}
    [%{"type" => "behaviour"}, item1, item2] = search_data(modules, config)["items"]
    assert item1["ref"] == "SearchFoo.html#c:handle_foo/0"
    assert item1["type"] == "callback"
    assert item1["title"] == "SearchFoo.handle_foo/0"
    assert item1["doc"] == "Handles _foo_."

    assert item2["ref"] == "SearchFoo.html#c:handle_foo/0-section"
    assert item2["type"] == "callback"
    assert item2["title"] == "Section - SearchFoo.handle_foo/0"
    assert item2["doc"] == "Section content."
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
    [%{"type" => "module"}, item] = search_data(modules, config)["items"]
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
    [item1, item2] = search_data([], config)["items"]

    assert item1["ref"] == "readme.html"
    assert item1["type"] == "extras"
    assert item1["title"] == "Foo"
    assert item1["doc"] == "# Foo\n\n_Foo_ content."

    assert item2["ref"] == "readme.html#section-1-header"
    assert item2["type"] == "extras"
    assert item2["title"] == "Section 1 Header - Foo"
    assert item2["doc"] == "Section _1_ content."
  end

  test "drops vega-lite blocks", c do
    readme_path = "#{c.tmp_dir}/README.md"

    File.write!(readme_path, """
    # Foo

    _Foo_ content.

    ## Section 1 Header

    ```vega-lite
      graph
    ```

    Section _1_ content.
    """)

    config = %ExDoc.Config{output: "#{c.tmp_dir}/doc", extras: [readme_path]}
    [item1, item2] = search_data([], config)["items"]

    assert item1["ref"] == "readme.html"
    assert item1["type"] == "extras"
    assert item1["title"] == "Foo"
    assert item1["doc"] == "# Foo\n\n_Foo_ content."

    assert item2["ref"] == "readme.html#section-1-header"
    assert item2["type"] == "extras"
    assert item2["title"] == "Section 1 Header - Foo"
    assert item2["doc"] == "Section _1_ content."
  end

  defp search_data(modules, config) do
    {modules, []} = ExDoc.Retriever.docs_from_modules(modules, config)

    ExDoc.Formatter.HTML.run(modules, [], config)
    [path] = Path.wildcard(Path.join([config.output, "dist", "search_data-*.js"]))
    "searchData=" <> json = File.read!(path)
    Jason.decode!(json)
  end
end
