Mix.start()
Mix.shell(Mix.Shell.Process)

defmodule Mix.Tasks.DocsTest do
  # Cannot run concurrently due to Mix compile/deps calls
  use ExUnit.Case

  def run(args, opts) do
    Mix.Tasks.Docs.run(args, opts, &{&1, &2, &3})
  end

  test "inflects values from app and version" do
    assert [
             {"ex_doc", "0.1.0", [formatter: "html", deps: _, source_beam: _, app: _]},
             {"ex_doc", "0.1.0", [formatter: "epub", deps: _, source_beam: _, app: _]}
           ] = run([], app: :ex_doc, version: "0.1.0")
  end

  test "accepts multiple formatters from CLI" do
    assert [
             {"ex_doc", "0.1.0", [formatter: "html", deps: _, source_beam: _, app: _]},
             {"ex_doc", "0.1.0", [formatter: "epub", deps: _, source_beam: _, app: _]}
           ] = run(["-f", "html", "-f", "epub"], app: :ex_doc, version: "0.1.0")
  end

  test "accepts multiple formatters from config" do
    assert [
             {"ex_doc", "0.1.0",
              [formatter: "html", deps: _, source_beam: _, app: _, formatters: _]},
             {"ex_doc", "0.1.0",
              [formatter: "epub", deps: _, source_beam: _, app: _, formatters: _]}
           ] = run([], app: :ex_doc, version: "0.1.0", docs: [formatters: ["html", "epub"]])
  end

  test "uses the given name" do
    assert [
             {"ExDoc", "0.1.0", [formatter: "html", deps: _, source_beam: _, app: _]},
             {"ExDoc", "0.1.0", [formatter: "epub", deps: _, source_beam: _, app: _]}
           ] = run([], app: :ex_doc, version: "0.1.0", name: "ExDoc")
  end

  test "accepts modules in :main" do
    assert [
             {"ex_doc", "dev",
              [formatter: "html", deps: _, main: "Sample", source_beam: _, app: _]},
             {"ex_doc", "dev",
              [formatter: "epub", deps: _, main: "Sample", source_beam: _, app: _]}
           ] = run([], app: :ex_doc, docs: [main: Sample])
  end

  test "accepts files in :main" do
    assert [
             {"ex_doc", "dev",
              [formatter: "html", deps: _, source_beam: _, app: _, main: "another"]},
             {"ex_doc", "dev",
              [formatter: "epub", deps: _, source_beam: _, app: _, main: "another"]}
           ] = run([], app: :ex_doc, docs: [main: "another"])
  end

  test "accepts output in :output" do
    assert [
             {"ex_doc", "dev",
              [formatter: "html", deps: _, source_beam: _, app: _, output: "hello"]},
             {"ex_doc", "dev",
              [formatter: "epub", deps: _, source_beam: _, app: _, output: "hello"]}
           ] = run([], app: :ex_doc, docs: [output: "hello"])
  end

  test "parses output with lower preference than options" do
    assert [
             {"ex_doc", "dev",
              [formatter: "html", deps: _, source_beam: _, app: _, output: "world"]},
             {"ex_doc", "dev",
              [formatter: "epub", deps: _, source_beam: _, app: _, output: "world"]}
           ] = run(~w(-o world), app: :ex_doc, docs: [output: "world"])
  end

  test "includes dependencies" do
    assert [
             {"ex_doc", "dev", [formatter: "html", deps: deps, source_beam: _, app: _]},
             {"ex_doc", "dev", [formatter: "epub", deps: deps, source_beam: _, app: _]}
           ] = run([], app: :ex_doc, docs: [])

    assert List.keyfind(deps, Application.app_dir(:earmark_parser), 0) ==
             {Application.app_dir(:earmark_parser),
              "https://hexdocs.pm/earmark_parser/#{Application.spec(:earmark_parser, :vsn)}/"}
  end

  test "allows custom dependency paths" do
    assert [
             {"ex_doc", "dev", [formatter: "html", deps: deps, source_beam: _, app: _]},
             {"ex_doc", "dev", [formatter: "epub", deps: deps, source_beam: _, app: _]}
           ] = run([], app: :ex_doc, docs: [deps: [earmark_parser: "foo"]])

    assert List.keyfind(deps, Application.app_dir(:earmark_parser), 0) ==
             {Application.app_dir(:earmark_parser), "foo"}
  end

  test "accepts lazy docs" do
    assert [
             {"ex_doc", "dev",
              [formatter: "html", deps: _, source_beam: _, app: _, main: "another"]},
             {"ex_doc", "dev",
              [formatter: "epub", deps: _, source_beam: _, app: _, main: "another"]}
           ] = run([], app: :ex_doc, docs: fn -> [main: "another"] end)
  end

  test "accepts options from root" do
    # accepted options are: `app`, `name`, `source_url`, `homepage_url`, `version`
    assert [
             {"ExDoc", "1.2.3-dev",
              [
                formatter: "html",
                deps: _,
                source_beam: _,
                homepage_url: "http://elixir-lang.org",
                source_url: "https://github.com/elixir-lang/ex_doc",
                app: _
              ]},
             {"ExDoc", "1.2.3-dev",
              [
                formatter: "epub",
                deps: _,
                source_beam: _,
                homepage_url: "http://elixir-lang.org",
                source_url: "https://github.com/elixir-lang/ex_doc",
                app: _
              ]}
           ] =
             run([],
               app: :ex_doc,
               name: "ExDoc",
               source_url: "https://github.com/elixir-lang/ex_doc",
               homepage_url: "http://elixir-lang.org",
               version: "1.2.3-dev"
             )

    assert [{"ex_doc", "dev", _}, {"ex_doc", "dev", _}] = run([], app: :ex_doc)
  end

  test "supports umbrella project" do
    Mix.Project.in_project(:umbrella, "test/fixtures/umbrella", fn _mod ->
      [
        {"umbrella", "dev",
         [
           formatter: "html",
           deps: _,
           siblings: [:bar, :foo],
           source_beam: _,
           app: _
         ]},
        {"umbrella", "dev",
         [
           formatter: "epub",
           deps: _,
           siblings: [:bar, :foo],
           source_beam: _,
           app: _
         ]}
      ] = run([], app: :umbrella, apps_path: "apps/", docs: [])
    end)
  end

  test "supports umbrella project with ignore_apps" do
    Mix.Project.in_project(:umbrella, "test/fixtures/umbrella", fn _mod ->
      [
        {"umbrella", "dev",
         [
           formatter: "html",
           deps: _,
           siblings: [:bar],
           source_beam: _,
           app: _,
           ignore_apps: [:foo]
         ]},
        {"umbrella", "dev",
         [
           formatter: "epub",
           deps: _,
           siblings: [:bar],
           source_beam: _,
           app: _,
           ignore_apps: [:foo]
         ]}
      ] = run([], app: :umbrella, apps_path: "apps/", docs: [ignore_apps: [:foo]])
    end)
  end
end
