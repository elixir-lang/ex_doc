Mix.start()
Mix.shell(Mix.Shell.Process)

defmodule Mix.Tasks.DocsTest do
  # Cannot run concurrently due to Mix compile/deps calls
  use ExUnit.Case, async: false

  @moduletag :tmp_dir

  def run(context, args, opts) do
    opts = Keyword.put_new(opts, :output, context[:tmp_dir])
    Mix.Tasks.Docs.run(args, opts, &{&1, &2, &3})
  end

  test "inflects values from app and version", context do
    assert [
             {"ex_doc", "0.1.0",
              [formatter: "html", deps: _, apps: _, source_beam: _, proglang: :elixir]},
             {"ex_doc", "0.1.0",
              [formatter: "epub", deps: _, apps: _, source_beam: _, proglang: :elixir]}
           ] = run(context, [], app: :ex_doc, version: "0.1.0")
  end

  test "accepts multiple formatters from CLI", context do
    assert [
             {"ex_doc", "0.1.0",
              [formatter: "html", deps: _, apps: _, source_beam: _, proglang: :elixir]},
             {"ex_doc", "0.1.0",
              [formatter: "epub", deps: _, apps: _, source_beam: _, proglang: :elixir]}
           ] = run(context, ["-f", "html", "-f", "epub"], app: :ex_doc, version: "0.1.0")
  end

  test "accepts multiple formatters from config", context do
    assert [
             {"ex_doc", "0.1.0",
              [
                formatter: "html",
                deps: _,
                apps: _,
                source_beam: _,
                formatters: _,
                proglang: :elixir
              ]},
             {"ex_doc", "0.1.0",
              [
                formatter: "epub",
                deps: _,
                apps: _,
                source_beam: _,
                formatters: _,
                proglang: :elixir
              ]}
           ] =
             run(context, [],
               app: :ex_doc,
               version: "0.1.0",
               docs: [formatters: ["html", "epub"]]
             )
  end

  test "uses the given name", context do
    assert [
             {"ExDoc", "0.1.0",
              [formatter: "html", deps: _, apps: _, source_beam: _, proglang: :elixir]},
             {"ExDoc", "0.1.0",
              [formatter: "epub", deps: _, apps: _, source_beam: _, proglang: :elixir]}
           ] = run(context, [], app: :ex_doc, version: "0.1.0", name: "ExDoc")
  end

  test "accepts modules in :main", context do
    assert [
             {"ex_doc", "dev",
              [
                formatter: "html",
                deps: _,
                main: "Sample",
                apps: _,
                source_beam: _,
                proglang: :elixir
              ]},
             {"ex_doc", "dev",
              [
                formatter: "epub",
                deps: _,
                main: "Sample",
                apps: _,
                source_beam: _,
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, docs: [main: Sample])
  end

  test "accepts files in :main", context do
    assert [
             {"ex_doc", "dev",
              [
                formatter: "html",
                deps: _,
                apps: _,
                source_beam: _,
                main: "another",
                proglang: :elixir
              ]},
             {"ex_doc", "dev",
              [
                formatter: "epub",
                deps: _,
                apps: _,
                source_beam: _,
                main: "another",
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, docs: [main: "another"])
  end

  test "accepts output in :output", %{tmp_dir: tmp_dir} = context do
    [{_, _, html_options}, {_, _, epub_options}] =
      run_results = run(context, [], app: :ex_doc, docs: [output: tmp_dir <> "/hello"])

    assert [
             {"ex_doc", "dev",
              [formatter: "html", deps: _, apps: _, source_beam: _, output: _, proglang: :elixir]},
             {"ex_doc", "dev",
              [formatter: "epub", deps: _, apps: _, source_beam: _, output: _, proglang: :elixir]}
           ] = run_results

    assert html_options[:output] == "#{tmp_dir}/hello"
    assert epub_options[:output] == "#{tmp_dir}/hello"
  end

  test "parses output with lower preference than options", %{tmp_dir: tmp_dir} = context do
    output = tmp_dir <> "/world"

    [{_, _, html_options}, {_, _, epub_options}] =
      run_results = run(context, ["-o", "#{output}"], app: :ex_doc, docs: [output: output])

    assert [
             {"ex_doc", "dev",
              [formatter: "html", deps: _, apps: _, source_beam: _, output: _, proglang: :elixir]},
             {"ex_doc", "dev",
              [formatter: "epub", deps: _, apps: _, source_beam: _, output: _, proglang: :elixir]}
           ] = run_results

    assert html_options[:output] == "#{tmp_dir}/world"
    assert epub_options[:output] == "#{tmp_dir}/world"
  end

  test "includes dependencies", context do
    assert [
             {"ex_doc", "dev",
              [formatter: "html", deps: deps, apps: _, source_beam: _, proglang: :elixir]},
             {"ex_doc", "dev",
              [formatter: "epub", deps: deps, apps: _, source_beam: _, proglang: :elixir]}
           ] = run(context, [], app: :ex_doc, docs: [])

    assert List.keyfind(deps, :earmark_parser, 0) ==
             {:earmark_parser,
              "https://hexdocs.pm/earmark_parser/#{Application.spec(:earmark_parser, :vsn)}/"}
  end

  test "allows custom dependency paths", context do
    assert [
             {"ex_doc", "dev",
              [formatter: "html", deps: deps, apps: _, source_beam: _, proglang: :elixir]},
             {"ex_doc", "dev",
              [formatter: "epub", deps: deps, apps: _, source_beam: _, proglang: :elixir]}
           ] = run(context, [], app: :ex_doc, docs: [deps: [earmark_parser: "foo"]])

    assert List.keyfind(deps, :earmark_parser, 0) ==
             {:earmark_parser, "foo"}
  end

  test "accepts lazy docs", context do
    assert [
             {"ex_doc", "dev",
              [
                formatter: "html",
                deps: _,
                apps: _,
                source_beam: _,
                main: "another",
                proglang: :elixir
              ]},
             {"ex_doc", "dev",
              [
                formatter: "epub",
                deps: _,
                apps: _,
                source_beam: _,
                main: "another",
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, docs: fn -> [main: "another"] end)
  end

  test "accepts options from root", context do
    # accepted options are: `app`, `name`, `source_url`, `homepage_url`, `version`
    assert [
             {"ExDoc", "1.2.3-dev",
              [
                formatter: "html",
                deps: _,
                apps: _,
                source_beam: _,
                homepage_url: "https://elixir-lang.org",
                source_url: "https://github.com/elixir-lang/ex_doc",
                proglang: :elixir
              ]},
             {"ExDoc", "1.2.3-dev",
              [
                formatter: "epub",
                deps: _,
                apps: _,
                source_beam: _,
                homepage_url: "https://elixir-lang.org",
                source_url: "https://github.com/elixir-lang/ex_doc",
                proglang: :elixir
              ]}
           ] =
             run(context, [],
               app: :ex_doc,
               name: "ExDoc",
               source_url: "https://github.com/elixir-lang/ex_doc",
               homepage_url: "https://elixir-lang.org",
               version: "1.2.3-dev",
               proglang: :elixir
             )

    assert [{"ex_doc", "dev", _}, {"ex_doc", "dev", _}] = run(context, [], app: :ex_doc)
  end

  test "supports umbrella project", context do
    Mix.Project.in_project(:umbrella, "test/fixtures/umbrella", fn _mod ->
      [
        {"umbrella", "dev",
         [
           formatter: "html",
           deps: _,
           apps: [:bar, :foo],
           source_beam: _,
           proglang: :elixir
         ]},
        {"umbrella", "dev",
         [
           formatter: "epub",
           deps: _,
           apps: [:bar, :foo],
           source_beam: _,
           proglang: :elixir
         ]}
      ] = run(context, [], app: :umbrella, apps_path: "apps/", docs: [])
    end)
  end

  test "supports umbrella project with ignore_apps", context do
    Mix.Project.in_project(:umbrella, "test/fixtures/umbrella", fn _mod ->
      [
        {"umbrella", "dev",
         [
           formatter: "html",
           deps: _,
           apps: [:bar],
           source_beam: _,
           ignore_apps: [:foo],
           proglang: :elixir
         ]},
        {"umbrella", "dev",
         [
           formatter: "epub",
           deps: _,
           apps: [:bar],
           source_beam: _,
           ignore_apps: [:foo],
           proglang: :elixir
         ]}
      ] = run(context, [], app: :umbrella, apps_path: "apps/", docs: [ignore_apps: [:foo]])
    end)
  end
end
