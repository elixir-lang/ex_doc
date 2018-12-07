defmodule ExDoc.Formatter.HTMLTest do
  use ExUnit.Case

  import ExUnit.CaptureIO
  alias ExDoc.Formatter.HTML
  alias ExDoc.Markdown.DummyProcessor

  setup do
    File.rm_rf(output_dir())
    File.mkdir_p!(output_dir())
  end

  defp output_dir do
    Path.expand("../../tmp/html", __DIR__)
  end

  defp beam_dir do
    Path.expand("../../tmp/beam", __DIR__)
  end

  defp read_wildcard!(path) do
    [file] = Path.wildcard(path)
    File.read!(file)
  end

  # The following module attributes contain the values for user-required content.
  # Content required by the custom markdown processor is defined in its own module,
  # and will be accessed as `DummyProcessor.before_closing_*_tag(:html)`
  @before_closing_head_tag_content_html "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-HEAD-TAG-HTML</dont-escape>"
  @before_closing_body_tag_content_html "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-BODY-TAG-HTML</dont-escape>"
  @before_closing_head_tag_content_epub "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-HEAD-TAG-EPUB</dont-escape>"
  @before_closing_body_tag_content_epub "UNIQUE:<dont-escape>&copy;BEFORE-CLOSING-BODY-TAG-EPUB</dont-escape>"

  defp before_closing_head_tag(:html), do: @before_closing_head_tag_content_html
  defp before_closing_head_tag(:epub), do: @before_closing_head_tag_content_epub

  defp before_closing_body_tag(:html), do: @before_closing_body_tag_content_html
  defp before_closing_body_tag(:epub), do: @before_closing_body_tag_content_epub

  defp doc_config do
    [
      project: "Elixir",
      version: "1.0.1",
      formatter: "html",
      assets: "test/tmp/html_assets",
      output: output_dir(),
      source_root: beam_dir(),
      source_beam: beam_dir(),
      logo: "test/fixtures/elixir.png",
      extras: ["test/fixtures/README.md"]
    ]
  end

  defp doc_config(config) do
    Keyword.merge(doc_config(), config)
  end

  defp generate_docs(config) do
    config = Keyword.put_new(config, :skip_undefined_reference_warnings_on, ["Warnings"])
    ExDoc.generate_docs(config[:project], config[:version], config)
  end

  test "normalizes options" do
    # 1. Check for output dir having trailing "/" stripped
    # 2. Check for default [main: "api-reference"]
    generate_docs(doc_config(output: "#{output_dir()}//", main: nil))

    content = File.read!("#{output_dir()}/index.html")
    assert content =~ ~r{<meta http-equiv="refresh" content="0; url=api-reference.html">}
    assert File.regular?("#{output_dir()}/api-reference.html")

    # 3. main as index is not allowed
    config = doc_config(main: "index")

    assert_raise ArgumentError,
                 ~S("main" cannot be set to "index", otherwise it will recursively link to itself),
                 fn -> generate_docs(config) end
  end

  describe "strip_tags" do
    test "removes html tags from text leaving the content" do
      assert HTML.strip_tags("<em>Hello</em> World!<br/>") == "Hello World!"
      assert HTML.strip_tags("Go <a href=\"#top\" class='small' disabled>back</a>") == "Go back"
      assert HTML.strip_tags("Git opts (<code class=\"inline\">:git</code>)") == "Git opts (:git)"
    end
  end

  describe "text_to_id" do
    test "id generation" do
      assert HTML.text_to_id("“Stale”") == "stale"
      assert HTML.text_to_id("José") == "josé"
      assert HTML.text_to_id(" a - b ") == "a-b"
      assert HTML.text_to_id(" ☃ ") == ""
      assert HTML.text_to_id(" &sup2; ") == ""
      assert HTML.text_to_id(" &#9180; ") == ""
      assert HTML.text_to_id("Git opts (<code class=\"inline\">:git</code>)") == "git-opts-git"
    end
  end

  test "warns when generating an index.html file with an invalid redirect" do
    output =
      capture_io(:stderr, fn ->
        generate_docs(doc_config(main: "Unknown"))
      end)

    assert output == "warning: index.html redirects to Unknown.html, which does not exist\n"
    assert File.regular?("#{output_dir()}/index.html")
    refute File.regular?("#{output_dir()}/Unknown.html")
  end

  test "warns on undefined functions" do
    output =
      capture_io(:stderr, fn ->
        generate_docs(doc_config(skip_undefined_reference_warnings_on: []))
      end)

    assert output =~ "Warnings.bar/0 is not found (parsing Warnings docs)"
    assert output =~ "Warnings.bar/0 is not found (parsing Warnings.foo/0 docs)"
    assert output =~ "Warnings.bar/0 is not found (parsing c:Warnings.handle_foo/0 docs)"
    assert output =~ "Warnings.bar/0 is not found (parsing t:Warnings.t/0 docs)"
  end

  test "generates headers for index.html and module pages" do
    generate_docs(doc_config(main: "RandomError"))
    content_index = File.read!("#{output_dir()}/index.html")
    content_module = File.read!("#{output_dir()}/RandomError.html")

    # Regular Expressions
    re = %{
      shared: %{
        charset: ~r{<meta charset="utf-8">},
        generator: ~r{<meta name="generator" content="ExDoc v#{ExDoc.version()}">}
      },
      index: %{
        title: ~r{<title>Elixir v1.0.1 – Documentation</title>},
        index: ~r{<meta name="robots" content="noindex"},
        refresh: ~r{<meta http-equiv="refresh" content="0; url=RandomError.html">}
      },
      module: %{
        title: ~r{<title>RandomError – Elixir v1.0.1</title>},
        viewport: ~r{<meta name="viewport" content="width=device-width, initial-scale=1.0">},
        x_ua: ~r{<meta http-equiv="x-ua-compatible" content="ie=edge">}
      }
    }

    assert content_index =~ re[:shared][:charset]
    assert content_index =~ re[:shared][:generator]
    assert content_index =~ re[:index][:title]
    assert content_index =~ re[:index][:index]
    assert content_index =~ re[:index][:refresh]
    refute content_index =~ re[:module][:title]
    refute content_index =~ re[:module][:viewport]
    refute content_index =~ re[:module][:x_ua]

    assert content_module =~ re[:shared][:charset]
    assert content_module =~ re[:shared][:generator]
    assert content_module =~ re[:module][:title]
    assert content_module =~ re[:module][:viewport]
    assert content_module =~ re[:module][:x_ua]
    refute content_module =~ re[:index][:title]
    refute content_module =~ re[:index][:index]
    refute content_module =~ re[:index][:refresh]
  end

  test "generates in default directory with redirect index.html file" do
    generate_docs(doc_config())

    assert File.regular?("#{output_dir()}/CompiledWithDocs.html")
    assert File.regular?("#{output_dir()}/CompiledWithDocs.Nested.html")

    assert [_] = Path.wildcard("#{output_dir()}/dist/html-*.css")
    assert [_] = Path.wildcard("#{output_dir()}/dist/html-*.js")
    assert [] = Path.wildcard("#{output_dir()}/another_dir/dist/html-*.js.map")

    content = File.read!("#{output_dir()}/index.html")
    assert content =~ ~r{<meta http-equiv="refresh" content="0; url=api-reference.html">}
  end

  test "generates all listing files" do
    generate_docs(doc_config())

    content = read_wildcard!("#{output_dir()}/dist/sidebar_items-*.js")
    assert content =~ ~r{"id":"CompiledWithDocs","title":"CompiledWithDocs"}ms
    assert content =~ ~r("id":"CompiledWithDocs".*"key":"functions".*"example/2")ms
    assert content =~ ~r{"id":"CompiledWithDocs\.Nested","title":"CompiledWithDocs\.Nested"}ms

    assert content =~ ~r{"id":"UndefParent\.Nested","title":"UndefParent\.Nested"}ms
    refute content =~ ~r{"id":"UndefParent\.Undocumented"}ms

    assert content =~ ~r{"id":"CustomBehaviourOne","title":"CustomBehaviourOne"}ms
    assert content =~ ~r{"id":"CustomBehaviourTwo","title":"CustomBehaviourTwo"}ms
    assert content =~ ~r{"id":"RandomError","title":"RandomError"}ms
    assert content =~ ~r{"id":"CustomProtocol","title":"CustomProtocol"}ms
    assert content =~ ~r{"id":"Mix\.Tasks\.TaskWithDocs","title":"task_with_docs"}ms
  end

  test "generates the api reference file" do
    generate_docs(doc_config())

    content = File.read!("#{output_dir()}/api-reference.html")
    assert content =~ ~r{<a href="CompiledWithDocs.html">CompiledWithDocs</a>}
    assert content =~ ~r{<p>moduledoc</p>}
    assert content =~ ~r{<a href="CompiledWithDocs.Nested.html">CompiledWithDocs.Nested</a>}
    assert content =~ ~r{<a href="Mix.Tasks.TaskWithDocs.html">task_with_docs</a>}
  end

  test "groups modules by nesting" do
    doc_config()
    |> Keyword.put(:nest_modules_by_prefix, [Common.Nesting.Prefix])
    |> generate_docs()

    content = read_wildcard!("#{output_dir()}/dist/sidebar_items-*.js")

    assert content =~
             ~r{"id":"Common\.Nesting\.Prefix\.Foo","title":"Common\.Nesting\.Prefix\.Foo","nested_title":"Foo","nested_context":"Common\.Nesting\.Prefix"}ms

    assert content =~
             ~r{"id":"Common\.Nesting\.Prefix\.Bar","title":"Common\.Nesting\.Prefix\.Bar","nested_title":"Bar","nested_context":"Common\.Nesting\.Prefix"}ms
  end

  describe "generates logo" do
    test "overriding previous entries" do
      File.mkdir_p!("#{output_dir()}/assets")
      File.touch!("#{output_dir()}/assets/logo.png")
      generate_docs(doc_config(logo: "test/fixtures/elixir.png"))
      assert File.read!("#{output_dir()}/assets/logo.png") != ""
    end

    test "fails when logo is not an allowed format" do
      config = doc_config(logo: "README.md")

      assert_raise ArgumentError,
                   "image format not recognized, allowed formats are: .jpg, .png",
                   fn -> generate_docs(config) end
    end
  end

  describe "canonical URL" do
    test "is included when canonical options is specified" do
      config =
        doc_config(extras: ["test/fixtures/README.md"], canonical: "https://hexdocs.pm/elixir/")

      generate_docs(config)
      content = File.read!("#{output_dir()}/api-reference.html")
      assert content =~ ~r{<link rel="canonical" href="https://hexdocs.pm/elixir/}

      content = File.read!("#{output_dir()}/readme.html")
      assert content =~ ~r{<link rel="canonical" href="https://hexdocs.pm/elixir/}
    end

    test "is not included when canonical is nil" do
      config = doc_config(canonical: nil)
      generate_docs(config)
      content = File.read!("#{output_dir()}/api-reference.html")
      refute content =~ ~r{<link rel="canonical" href="}
    end
  end

  describe "generates extras" do
    test "alongside other content" do
      config = doc_config(main: "readme")
      generate_docs(config)

      content = File.read!("#{output_dir()}/index.html")
      assert content =~ ~r{<meta http-equiv="refresh" content="0; url=readme.html">}

      content = File.read!("#{output_dir()}/readme.html")
      assert content =~ ~r{<title>README [^<]*</title>}

      assert content =~
               ~r{<h2 id="header-sample" class="section-heading">.*<a href="#header-sample" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>.*<code(\sclass="inline")?>Header</code> sample.*</h2>}ms

      assert content =~
               ~r{<h2 id="more-than" class="section-heading">.*<a href="#more-than" class="hover-link"><span class="icon-link" aria-hidden="true"></span></a>.*more &gt; than.*</h2>}ms

      assert content =~ ~r{<a href="RandomError.html"><code(\sclass="inline")?>RandomError</code>}

      assert content =~
               ~r{<a href="CustomBehaviourImpl.html#hello/1"><code(\sclass="inline")?>CustomBehaviourImpl.hello/1</code>}

      assert content =~
               ~r{<a href="TypesAndSpecs.Sub.html"><code(\sclass="inline")?>TypesAndSpecs.Sub</code></a>}

      assert content =~
               ~r{<a href="TypesAndSpecs.Sub.html"><code(\sclass="inline")?>TypesAndSpecs.Sub</code></a>}

      assert content =~
               ~r{<a href="https://hexdocs.pm/elixir/Kernel.html#is_atom/1"><code(\sclass="inline")?>is_atom/1</code></a>}

      assert content =~
               ~r{<a href="https://hexdocs.pm/elixir/Kernel.html#==/2"><code(\sclass="inline")?>==/2</code></a>}

      assert content =~
               ~r{<a href="https://hexdocs.pm/elixir/Kernel.html#===/2"><code(\sclass="inline")?>===</code></a>}

      assert content =~
               ~r{<a href="https://hexdocs.pm/elixir/typespecs.html#basic-types"><code(\sclass="inline")?>atom/0</code></a>}

      assert content =~
               ~r{<a href="https://hexdocs.pm/mix/Mix.Tasks.Compile.Elixir.html"><code(\sclass="inline")?>mix compile.elixir</code></a>}
    end

    test "without any other content" do
      generate_docs(doc_config(source_root: "unknown", source_beam: "unknown"))

      content = read_wildcard!("#{output_dir()}/dist/sidebar_items-*.js")
      assert content =~ ~s("modules":[])
      assert content =~ ~s("exceptions":[])

      assert content =~
               ~s("extras":[{"id":"api-reference","title":"API Reference","group":"","headers":[]},)

      assert content =~
               ~s({"id":"readme","title":"README","group":"","headers":[{"id":"Header sample","anchor":"header-sample"},)
    end

    test "containing settext headers while discarding links on header" do
      generate_docs(
        doc_config(
          source_root: "unknown",
          source_beam: "unknown",
          extras: ["test/fixtures/ExtraPageWithSettextHeader.md"]
        )
      )

      content = read_wildcard!("#{output_dir()}/dist/sidebar_items-*.js")

      assert content =~
               ~s("extras":[{"id":"api-reference","title":"API Reference","group":"","headers":[]},)

      assert content =~
               ~s({"id":"extrapagewithsettextheader","title":"Extra Page Title","group":"",) <>
                 ~s("headers":[{"id":"Section One","anchor":"section-one"},{"id":"Section Two","anchor":"section-two"}]}])
    end

    test "with custom names" do
      generate_docs(
        doc_config(extras: ["test/fixtures/README.md": [filename: "GETTING-STARTED"]])
      )

      refute File.regular?("#{output_dir()}/readme.html")
      content = File.read!("#{output_dir()}/GETTING-STARTED.html")
      assert content =~ ~r{<title>README [^<]*</title>}
      content = read_wildcard!("#{output_dir()}/dist/sidebar_items-*.js")
      assert content =~ ~r{"id":"GETTING-STARTED","title":"README"}
    end

    test "with custom title" do
      generate_docs(doc_config(extras: ["test/fixtures/README.md": [title: "Getting Started"]]))
      content = File.read!("#{output_dir()}/readme.html")
      assert content =~ ~r{<title>Getting Started – Elixir v1.0.1</title>}
      content = read_wildcard!("#{output_dir()}/dist/sidebar_items-*.js")
      assert content =~ ~r{"id":"readme","title":"Getting Started","group":""}
    end

    test "with custom groups" do
      extra_config = [
        extras: ["test/fixtures/README.md"],
        groups_for_extras: [Intro: ~r/fixtures\/READ.?/]
      ]

      generate_docs(doc_config(extra_config))
      content = read_wildcard!("#{output_dir()}/dist/sidebar_items-*.js")
      assert content =~ ~r{"id":"readme","title":"README","group":"Intro"}
    end

    test "with auto-extracted titles" do
      generate_docs(doc_config(extras: ["test/fixtures/ExtraPage.md"]))
      content = File.read!("#{output_dir()}/extrapage.html")
      assert content =~ ~r{<title>Extra Page Title – Elixir v1.0.1</title>}
      content = read_wildcard!("#{output_dir()}/dist/sidebar_items-*.js")
      assert content =~ ~r{"id":"extrapage","title":"Extra Page Title"}
    end

    test "without api-reference" do
      generate_docs(
        doc_config(api_reference: false, extras: ["test/fixtures/README.md"], main: "readme")
      )

      refute File.exists?("#{output_dir()}/api-reference.html")
      content = read_wildcard!("#{output_dir()}/dist/sidebar_items-*.js")
      refute content =~ ~r{"id":"api-reference","title":"API Reference"}
    end
  end

  describe ".build" do
    test "stores generated content" do
      config = doc_config(extras: ["test/fixtures/README.md"], logo: "test/fixtures/elixir.png")
      generate_docs(config)
      content = File.read!("#{output_dir()}/.build")
      assert content =~ ~r(^readme\.html$)m
      assert content =~ ~r(^api-reference\.html$)m
      assert content =~ ~r(^dist/sidebar_items-[\w]{10}\.js$)m
      assert content =~ ~r(^dist/html-[\w]{20}\.js$)m
      assert content =~ ~r(^dist/html-[\w]{20}\.css$)m
      assert content =~ ~r(^assets/logo\.png$)m
      assert content =~ ~r(^index\.html$)m
      assert content =~ ~r(^404\.html$)m
    end

    test "does not delete files not listed in .build" do
      keep = "#{output_dir()}/keep"
      config = doc_config()
      generate_docs(config)
      File.touch!(keep)
      generate_docs(config)
      assert File.exists?(keep)
      content = File.read!("#{output_dir()}/.build")
      refute content =~ ~r{keep}
    end
  end

  describe "before_closing_*_tags" do
    # There are 3 possibilities for the `before_closing_*_tags`:
    # - required by the user alone
    # - required by the markdown processor alone
    # - required by both the markdown processor and the user
    # We will test the three possibilities independently

    # 1. Required by the user alone
    test "required by the user are placed in the right place" do
      generate_docs(
        doc_config(
          before_closing_head_tag: &before_closing_head_tag/1,
          before_closing_body_tag: &before_closing_body_tag/1
        )
      )

      content = File.read!("#{output_dir()}/api-reference.html")
      assert content =~ ~r[#{@before_closing_head_tag_content_html}\s*</head>]
      assert content =~ ~r[#{@before_closing_body_tag_content_html}\s*</body>]

      content = File.read!("#{output_dir()}/readme.html")
      assert content =~ ~r[#{@before_closing_head_tag_content_html}\s*</head>]
      assert content =~ ~r[#{@before_closing_body_tag_content_html}\s*</body>]

      content = File.read!("#{output_dir()}/api-reference.html")
      assert not (content =~ ~r[#{DummyProcessor.before_closing_head_tag(:html)}\s*</head>])
      assert not (content =~ ~r[#{DummyProcessor.before_closing_body_tag(:html)}\s*</body>])
    end

    # 2. Required by the markdown processor alone
    test "required by the markdown processor are placed in the right place" do
      generate_docs(doc_config(markdown_processor: DummyProcessor))

      content = File.read!("#{output_dir()}/api-reference.html")
      assert content =~ ~r[#{DummyProcessor.before_closing_head_tag(:html)}\s*</head>]
      assert content =~ ~r[#{DummyProcessor.before_closing_body_tag(:html)}\s*</body>]

      content = File.read!("#{output_dir()}/readme.html")
      assert content =~ ~r[#{DummyProcessor.before_closing_head_tag(:html)}\s*</head>]
      assert content =~ ~r[#{DummyProcessor.before_closing_body_tag(:html)}\s*</body>]

      content = File.read!("#{output_dir()}/api-reference.html")
      assert not (content =~ ~r[#{@before_closing_head_tag_content_html}\s*</head>])
      assert not (content =~ ~r[#{@before_closing_body_tag_content_html}\s*</body>])
    end

    test "required by (1) the user and (2) the markdown processor are placed in the right place" do
      generate_docs(
        doc_config(
          before_closing_head_tag: &before_closing_head_tag/1,
          before_closing_body_tag: &before_closing_body_tag/1,
          markdown_processor: DummyProcessor
        )
      )

      head_regex =
        ~r[#{DummyProcessor.before_closing_head_tag(:html)}\s*#{
          @before_closing_head_tag_content_html
        }\s*</head>]

      body_regex =
        ~r[#{DummyProcessor.before_closing_body_tag(:html)}\s*#{
          @before_closing_body_tag_content_html
        }\s*</body>]

      content = File.read!("#{output_dir()}/api-reference.html")
      assert content =~ head_regex
      assert content =~ body_regex

      content = File.read!("#{output_dir()}/readme.html")
      assert content =~ head_regex
      assert content =~ body_regex
    end
  end

  describe "assets" do
    # There are 3 possibilities when requiring:
    # - required by the user alone
    # - required by the markdown processor alone
    # - required by both the markdown processor and the user
    # We will test the three possibilities independently

    # 1. Required by the user alone
    test "required by the user end up in the right place" do
      File.mkdir_p!("test/tmp/html_assets/hello")
      File.touch!("test/tmp/html_assets/hello/world")
      generate_docs(doc_config(assets: "test/tmp/html_assets", logo: "test/fixtures/elixir.png"))
      assert File.regular?("#{output_dir()}/assets/logo.png")
      assert File.regular?("#{output_dir()}/assets/hello/world")
    after
      File.rm_rf!("test/tmp/html_assets")
    end

    # 2. Required by the markdown processor alone
    test "required by the markdown processor end up in the right place" do
      generate_docs(doc_config(markdown_processor: DummyProcessor))
      # Test the assets added by the markdown processor
      for [{filename, content}] <- DummyProcessor.assets(:html) do
        # Filename matches
        assert File.regular?("#{output_dir()}/#{filename}")
        # Content matches
        assert File.read!("#{output_dir()}/#{filename}") == content
      end
    end

    # 3. Required by both the user and the markdown processor
    test "required by the user and markdown processor end up in the right place" do
      File.mkdir_p!("test/tmp/html_assets/hello")
      File.touch!("test/tmp/html_assets/hello/world")

      generate_docs(
        doc_config(
          assets: "test/tmp/html_assets",
          markdown_processor: DummyProcessor,
          logo: "test/fixtures/elixir.png"
        )
      )

      # Test the assets added by the markdown processor
      for [{filename, content}] <- DummyProcessor.assets(:html) do
        # Filename matches
        assert File.regular?("#{output_dir()}/#{filename}")
        # Content matches
        assert File.read!("#{output_dir()}/#{filename}") == content
      end
    end
  end
end
