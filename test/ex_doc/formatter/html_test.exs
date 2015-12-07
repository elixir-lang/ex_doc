defmodule ExDoc.Formatter.HTMLTest do
  use ExUnit.Case

  setup do
    File.rm_rf(output_dir)
    File.mkdir_p!(output_dir)
  end

  defp output_dir do
    Path.expand("../../tmp/doc", __DIR__)
  end

  defp beam_dir do
    Application.app_dir(:ex_doc, "ebin")
  end

  defp doc_config do
    [project: "Elixir",
     version: "1.0.1",
     formatter: "html",
     output: output_dir,
     source_root: beam_dir,
     source_beam: beam_dir,
     logo: "test/fixtures/elixir.png",
     extras: ["test/fixtures/README.md"]]
  end

  defp doc_config(config) do
    Keyword.merge(doc_config, config)
  end

  defp generate_docs(config) do
    ExDoc.generate_docs(config[:project], config[:version], config)
  end

  test "guess url base on source_url and source_root options" do
    generate_docs doc_config(source_url: "https://github.com/elixir-lang/ex_doc", source_root: File.cwd!)
    content = File.read!("#{output_dir}/CompiledWithDocs.html")
    assert content =~ "https://github.com/elixir-lang/ex_doc/blob/master/test/fixtures/compiled_with_docs.ex#L13"

    generate_docs doc_config(source_url: "https://bitbucket.org/elixir-lang/ex_doc", source_root: File.cwd!)
    content = File.read!("#{output_dir}/CompiledWithDocs.html")
    assert content =~ "https://bitbucket.org/elixir-lang/ex_doc/src/master/test/fixtures/compiled_with_docs.ex#cl-13"
  end

  test "find formatter when absolute path to module is given" do
    generate_docs doc_config(formatter: "ExDoc.Formatter.HTML")

    assert File.regular?("#{output_dir}/CompiledWithDocs.html")
  end

  test "run generates in default directory and redirect index.html file" do
    generate_docs(doc_config)

    assert File.regular?("#{output_dir}/CompiledWithDocs.html")
    assert File.regular?("#{output_dir}/CompiledWithDocs.Nested.html")

    content = File.read!("#{output_dir}/index.html")
    assert content =~ ~r{<meta http-equiv="refresh" content="0; url=api-reference.html">}
  end

  test "check headers for index.html and module pages" do
    generate_docs doc_config(main: "RandomError")
    content_index  = File.read!("#{output_dir}/index.html")
    content_module = File.read!("#{output_dir}/RandomError.html")

    # Regular Expressions
    re = %{
      shared: %{
        charset:   ~r{<meta charset="utf-8">},
        generator: ~r{<meta name="generator" content="ExDoc v#{ExDoc.version}">},
      },

      index: %{
        title:   ~r{<title>Elixir v1.0.1 – Documentation</title>},
        index:   ~r{<meta name="robots" content="noindex"},
        refresh: ~r{<meta http-equiv="refresh" content="0; url=RandomError.html">},
      },

      module: %{
        title:    ~r{<title>RandomError – Elixir v1.0.1</title>},
        viewport: ~r{<meta name="viewport" content="width=device-width, initial-scale=1.0">},
        x_ua:     ~r{<meta http-equiv="x-ua-compatible" content="ie=edge">},
      },
    }

    assert content_index  =~ re[:shared][:charset]
    assert content_index  =~ re[:shared][:generator]
    assert content_index  =~ re[:index][:title]
    assert content_index  =~ re[:index][:index]
    assert content_index  =~ re[:index][:refresh]
    refute content_index  =~ re[:module][:title]
    refute content_index  =~ re[:module][:viewport]
    refute content_index  =~ re[:module][:x_ua]

    assert content_module =~ re[:shared][:charset]
    assert content_module =~ re[:shared][:generator]
    assert content_module =~ re[:module][:title]
    assert content_module =~ re[:module][:viewport]
    assert content_module =~ re[:module][:x_ua]
    refute content_module =~ re[:index][:title]
    refute content_module =~ re[:index][:index]
    refute content_module =~ re[:index][:refresh]
  end

  test "run generates in specified output directory and redirect index.html file" do
    config = doc_config(output: "#{output_dir}/another_dir", main: "RandomError")
    generate_docs(config)

    assert File.regular?("#{output_dir}/another_dir/CompiledWithDocs.html")
    assert "#{output_dir}/another_dir/dist/app-*.css" |> Path.wildcard |> File.regular?
    assert "#{output_dir}/another_dir/dist/app-*.js" |> Path.wildcard |> File.regular?
    assert File.regular?("#{output_dir}/another_dir/RandomError.html")
    content = File.read!("#{output_dir}/another_dir/index.html")
    assert content =~ ~r{<meta http-equiv="refresh" content="0; url=RandomError.html">}
  end

  test "run generates all listing files" do
    generate_docs(doc_config)

    content = File.read!("#{output_dir}/dist/sidebar_items.json")
    assert content =~ ~r{"id":"CompiledWithDocs\"}ms
    assert content =~ ~r("id":"CompiledWithDocs".*"functions":.*"example/2")ms
    assert content =~ ~r{"id":"CompiledWithDocs\.Nested"}ms

    assert content =~ ~r{"id":"UndefParent\.Nested"}ms
    refute content =~ ~r{"id":"UndefParent\.Undocumented"}ms

    assert content =~ ~r{"id":"CustomBehaviourOne"}ms
    assert content =~ ~r{"id":"CustomBehaviourTwo"}ms
    assert content =~ ~r{"id":"RandomError"}ms
    assert content =~ ~r{"id":"CustomProtocol"}ms
  end

  test "run generates empty listing files only with extras" do
    generate_docs(doc_config(source_root: "unknown", source_beam: "unknown"))

    content = File.read!("#{output_dir}/dist/sidebar_items.json")
    assert content =~ ~s("modules":[])
    assert content =~ ~s("exceptions":[])
    assert content =~ ~s("protocols":[])
    assert content =~ ~s("extras":[{"id":"api-reference","title":"API Reference","headers":[]},)
    assert content =~ ~s({"id":"readme","title":"README","headers":[{"id":" Header sample","anchor":"Header-sample"}]})
  end

  test "run generates the api reference file" do
    generate_docs(doc_config)

    content = File.read!("#{output_dir}/api-reference.html")
    assert content =~ ~r{<a href="CompiledWithDocs.html">CompiledWithDocs</a>}
    assert content =~ ~r{<p>moduledoc</p>}
    assert content =~ ~r{<a href="CompiledWithDocs.Nested.html">CompiledWithDocs.Nested</a>}
  end

  test "run generates the readme file" do
    config = doc_config([main: "README"])
    generate_docs(config)

    content = File.read!("#{output_dir}/index.html")
    assert content =~ ~r{<meta http-equiv="refresh" content="0; url=README.html">}

    content = File.read!("#{output_dir}/readme.html")
    assert content =~ ~r{<title>README [^<]*</title>}
    assert content =~ ~r{<h2 id="Header-sample"> Header sample</h2>}
    assert content =~ ~r{<a href="RandomError.html"><code>RandomError</code>}
    assert content =~ ~r{<a href="CustomBehaviourImpl.html#hello/1"><code>CustomBehaviourImpl.hello/1</code>}
    assert content =~ ~r{<a href="TypesAndSpecs.Sub.html"><code>TypesAndSpecs.Sub</code></a>}
  end

  test "run should not generate the readme file" do
    generate_docs(doc_config(extras: []))
    refute File.regular?("#{output_dir}/README.html")
    content = File.read!("#{output_dir}/index.html")
    refute content =~ ~r{<title>README [^<]*</title>}
  end

  test "run should generate the readme input file as getting-started" do
    generate_docs(doc_config(extras: ["test/fixtures/README.md": [path: "GETTING-STARTED"]]))
    refute File.regular?("#{output_dir}/readme.html")
    content = File.read!("#{output_dir}/GETTING-STARTED.html")
    assert content =~ ~r{<title>README [^<]*</title>}
    content = File.read!("#{output_dir}/dist/sidebar_items.json")
    assert content =~ ~r{"id":"GETTING-STARTED","title":"README"}
  end

  test "run uses custom menu title" do
    generate_docs(doc_config(extras: ["test/fixtures/README.md": [title: "Getting Started"]]))
    content = File.read!("#{output_dir}/readme.html")
    assert content =~ ~r{<title>Getting Started – Elixir v1.0.1</title>}
    content = File.read!("#{output_dir}/dist/sidebar_items.json")
    assert content =~ ~r{"id":"readme","title":"Getting Started"}
   end

  test "run uses first <h1> as menu title" do
    generate_docs(doc_config(extras: ["test/fixtures/ExtraPage.md"]))
    content = File.read!("#{output_dir}/extrapage.html")
    assert content =~ ~r{<title>Extra Page Title – Elixir v1.0.1</title>}
    content = File.read!("#{output_dir}/dist/sidebar_items.json")
    assert content =~ ~r{"id":"extrapage","title":"Extra Page Title"}
  end

  test "run normalizes options" do
    # 1. Check for output dir having trailing "/" stripped
    # 2. Check for default [main: "api-reference"]
    generate_docs doc_config(output: "#{output_dir}//", main: nil)

    content = File.read!("#{output_dir}/index.html")
    assert content =~ ~r{<meta http-equiv="refresh" content="0; url=api-reference.html">}
    assert File.regular?("#{output_dir}/api-reference.html")

    # 3. main as index is not allowed
    config = doc_config([main: "index"])
    assert_raise ArgumentError,
                 ~S("main" cannot be set to "index", otherwise it will recursively link to itself),
                 fn -> generate_docs(config) end
  end

  test "run fails when logo is not an allowed format" do
    config = doc_config(logo: "README.md")
    assert_raise ArgumentError,
                 "image format not recognized, allowed formats are: .jpg, .png",
                 fn -> generate_docs(config) end
  end

  test "Generate some assets" do
    output = doc_config[:output]
    ExDoc.Formatter.HTML.generate_assets([{"test/fixtures/elixir.png", "images"}], output)

    assert File.regular?("#{output}/images/elixir.png")
  end
end
