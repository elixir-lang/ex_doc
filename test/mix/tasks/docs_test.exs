Mix.start()
Mix.shell(Mix.Shell.Process)

defmodule Mix.Tasks.DocsTest do
  # Cannot run concurrently due to Mix compile/deps calls
  use ExUnit.Case

  def run(args, opts) do
    Mix.Tasks.Docs.run(args, opts, &{&1, &2, &3})
  end

  test "inflects values from app and version" do
    assert [{"ex_doc", "0.1.0", [formatter: "html", deps: _, source_beam: _]}] =
           run([], [app: :ex_doc, version: "0.1.0"])
  end

  test "accepts multiple formatters from CLI" do
    assert [{"ex_doc", "0.1.0", [formatter: "html", deps: _, source_beam: _]},
            {"ex_doc", "0.1.0", [formatter: "epub", deps: _, source_beam: _]}] =
           run(["-f", "html", "-f", "epub"], [app: :ex_doc, version: "0.1.0"])
  end

  test "accepts multiple formatters from config" do
    assert [{"ex_doc", "0.1.0", [formatter: "html", deps: _, source_beam: _, formatters: _]},
            {"ex_doc", "0.1.0", [formatter: "epub", deps: _, source_beam: _, formatters: _]}] =
           run([], [app: :ex_doc, version: "0.1.0", docs: [formatters: ["html", "epub"]]])
  end

  test "uses the given name" do
    assert [{"ExDoc", "0.1.0", [formatter: "html", deps: _, source_beam: _]}] =
           run([], [app: :ex_doc, version: "0.1.0", name: "ExDoc"])
  end

  test "accepts modules in :main" do
    assert [{"ex_doc", "dev", [formatter: "html", deps: _, main: "Sample", source_beam: _, ]}] =
           run([], [app: :ex_doc, docs: [main: Sample]])
  end

  test "accepts files in :main" do
    assert [{"ex_doc", "dev", [formatter: "html", deps: _, source_beam: _, main: "another"]}] =
           run([], [app: :ex_doc, docs: [main: "another"]])
  end

  test "accepts output in :output" do
    assert [{"ex_doc", "dev", [formatter: "html", deps: _, source_beam: _, output: "hello"]}] =
           run([], [app: :ex_doc, docs: [output: "hello"]])
  end

  test "parses output with lower preference than options" do
    assert [{"ex_doc", "dev", [formatter: "html", deps: _, source_beam: _, output: "world"]}] =
           run(~w(-o world), [app: :ex_doc, docs: [output: "world"]])
  end

  test "includes dependencies" do
    assert [{"ex_doc", "dev", [formatter: "html", deps: deps, source_beam: _]}] =
           run([], [app: :ex_doc, docs: []])
    assert List.keyfind(deps, Application.app_dir(:earmark), 0) ==
           {Application.app_dir(:earmark), "https://hexdocs.pm/earmark/#{Application.spec(:earmark, :vsn)}/"}
  end

  test "allows custom dependency paths" do
    assert [{"ex_doc", "dev", [formatter: "html", deps: deps, source_beam: _]}] =
           run([], [app: :ex_doc, docs: [deps: [earmark: "foo"]]])
    assert List.keyfind(deps, Application.app_dir(:earmark), 0) ==
           {Application.app_dir(:earmark), "foo"}
  end

  test "accepts lazy docs" do
    assert [{"ex_doc", "dev", [formatter: "html", deps: _, source_beam: _, main: "another"]}] =
           run([], [app: :ex_doc, docs: fn -> [main: "another"] end])
  end

  test "accepts source_url from root" do
    assert [{"ex_doc", "dev", [formatter: "html", deps: _, source_beam: _,
                               source_url: "http://github.com/elixir-lang/ex_doc"]}] =
           run([], [app: :ex_doc, source_url: "http://github.com/elixir-lang/ex_doc"])
  end

  test "supports umbrella project" do
    Mix.Project.in_project(:umbrella, "test/fixtures/umbrella", fn _mod ->
      assert [{"umbrella", "dev", [formatter: "html", deps: deps, source_beam: _]}] =
             run([], [app: :umbrella, apps_path: "apps/"])

      assert List.keyfind(deps, Application.app_dir(:foo), 0) ==
             {Application.app_dir(:foo), "https://hexdocs.pm/foo/0.1.0/"}
      assert List.keyfind(deps, Application.app_dir(:bar), 0) ==
             {Application.app_dir(:bar), "https://hexdocs.pm/bar/0.1.0/"}
    end)
  end
end
