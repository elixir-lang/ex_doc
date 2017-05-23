Mix.start()
Mix.shell(Mix.Shell.Process)

defmodule Mix.Tasks.DocsTest do
  # Cannot run concurrently due to Mix compile/deps calls
  use ExUnit.Case

  def run(args, opts) do
    Mix.Tasks.Docs.run(args, opts, &generate_docs/3)
  end

  def generate_docs(project, vsn, options) when is_binary(project) and is_binary(vsn) and is_list(options) do
    config = ExDoc.build_config(project, vsn, options)
    {project, vsn, config}
  end

  test "inflects values from app and version" do
    assert [{"ex_doc", "0.1.0", %ExDoc.Config{formatter: "html"}}] =
           run([], [app: :ex_doc, version: "0.1.0"])
  end

  test "accepts multiple formatters from CLI" do
    assert [{"ex_doc", "0.1.0", %ExDoc.Config{formatter: "html"}},
            {"ex_doc", "0.1.0", %ExDoc.Config{formatter: "epub"}}] =
           run(["-f", "html", "-f", "epub"], [app: :ex_doc, version: "0.1.0"])
  end

  test "accepts multiple formatters from config" do
    assert [{"ex_doc", "0.1.0", %ExDoc.Config{formatter: "html"}},
            {"ex_doc", "0.1.0", %ExDoc.Config{formatter: "epub"}}] =
           run([], [app: :ex_doc, version: "0.1.0", docs: [formatters: ["html", "epub"]]])
  end

  test "uses the given name" do
    assert [{"ExDoc", "0.1.0", %ExDoc.Config{formatter: "html"}}] =
           run([], [app: :ex_doc, version: "0.1.0", name: "ExDoc"])
  end

  test "accepts modules in :main" do
    assert [{"ex_doc", "dev", %ExDoc.Config{formatter: "html", main: "Sample"}}] =
           run([], [app: :ex_doc, docs: [main: Sample]])
  end

  test "accepts files in :main" do
    assert [{"ex_doc", "dev", %ExDoc.Config{formatter: "html", main: "another"}}] =
           run([], [app: :ex_doc, docs: [main: "another"]])
  end

  test "accepts output in :output" do
    assert [{"ex_doc", "dev", %ExDoc.Config{formatter: "html", output: "hello"}}] =
           run([], [app: :ex_doc, docs: [output: "hello"]])
  end

  test "parses output with lower preference than options" do
    assert [{"ex_doc", "dev", %ExDoc.Config{formatter: "html", output: "world"}}] =
           run(~w(-o world), [app: :ex_doc, docs: [output: "world"]])
  end

  test "includes dependencies" do
    assert [{"ex_doc", "dev", %ExDoc.Config{formatter: "html", deps: deps}}] =
           run([], [app: :ex_doc, docs: []])
    assert List.keyfind(deps, Application.app_dir(:earmark), 0) ==
           {Application.app_dir(:earmark), "https://hexdocs.pm/earmark/#{Application.spec(:earmark, :vsn)}/"}
  end

  test "allows custom dependency paths" do
    assert [{"ex_doc", "dev", %ExDoc.Config{formatter: "html", deps: deps}}] =
           run([], [app: :ex_doc, docs: [deps: [earmark: "foo"]]])
    assert List.keyfind(deps, Application.app_dir(:earmark), 0) ==
           {Application.app_dir(:earmark), "foo"}
  end

  test "accepts lazy docs" do
    assert [{"ex_doc", "dev", %ExDoc.Config{formatter: "html", main: "another"}}] =
           run([], [app: :ex_doc, docs: fn -> [main: "another"] end])
  end

  describe "accepts options from root" do
    # accepted options are: `app`, `name`, `source_url`, `homepage_url`, `version`

    test "all options" do
      assert [{"ExDoc", "1.2.3-dev",
                %ExDoc.Config{
                  formatter: "html",
                  homepage_url: "http://elixir-lang.org",
                  project: "ExDoc",
                  source_url: "https://github.com/elixir-lang/ex_doc",
                  source_url_pattern: "https://github.com/elixir-lang/ex_doc/blob/master/%{path}#L%{line}",
                  version: "1.2.3-dev",
                }
              }] =
             run([], [app: :ex_doc,
                      name: "ExDoc",
                      source_url: "https://github.com/elixir-lang/ex_doc",
                      homepage_url: "http://elixir-lang.org",
                      version: "1.2.3-dev",
                     ])
    end

    test "app only" do
      assert [{"ex_doc", "dev", %ExDoc.Config{}}] = run([], [app: :ex_doc])
    end

    test "source_url" do
      assert [{"ex_doc", "dev",
               %ExDoc.Config{
                 formatter: "html",
                 source_url_pattern: "https://github.com/elixir-lang/ex_doc/blob/master/%{path}#L%{line}",
                 source_url: "https://github.com/elixir-lang/ex_doc",
              }}] =
             run([], [app: :ex_doc, source_url: "https://github.com/elixir-lang/ex_doc"])
    end
  end

  test "supports umbrella project" do
    Mix.Project.in_project(:umbrella, "test/fixtures/umbrella", fn _mod ->
      assert [{"umbrella", "dev", %ExDoc.Config{formatter: "html", deps: deps}}] =
             run([], [app: :umbrella, apps_path: "apps/"])

      assert List.keyfind(deps, Application.app_dir(:foo), 0) ==
             {Application.app_dir(:foo), "https://hexdocs.pm/foo/0.1.0/"}
      assert List.keyfind(deps, Application.app_dir(:bar), 0) ==
             {Application.app_dir(:bar), "https://hexdocs.pm/bar/0.1.0/"}
    end)
  end
end
