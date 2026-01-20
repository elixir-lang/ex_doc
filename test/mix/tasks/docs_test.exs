Mix.start()
Mix.shell(Mix.Shell.Process)

defmodule Mix.Tasks.DocsTest do
  # Cannot run concurrently due to Mix compile/deps calls
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  @moduletag :tmp_dir

  def run(context, args, opts, generator \\ &mock_generator/4) do
    opts = Keyword.put_new(opts, :output, context[:tmp_dir])
    Mix.Tasks.Docs.run(args, opts, generator)
  end

  defp mock_generator(project, version, source_beam, options) do
    formatter = options |> Keyword.get(:formatters, ["html"]) |> hd()

    [
      %{
        entrypoint: {project, version, source_beam, options},
        warned?: false,
        formatter: Module.concat(ExDoc.Formatter, String.upcase(formatter))
      }
    ]
  end

  def run_with_io(context, args, opts, generator)
      when is_list(args) and is_list(opts) and is_function(generator, 4) do
    opts = Keyword.put_new(opts, :output, context[:tmp_dir])

    with_io(fn ->
      run(context, args, opts, generator)
    end)
  end

  test "inflects values from app and version", context do
    assert [
             {"ex_doc", "0.1.0", _,
              [
                formatters: ["html", "markdown", "epub"],
                deps: _,
                apps: _,
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, version: "0.1.0")
  end

  test "accepts multiple formatters from CLI", context do
    assert [
             {"ex_doc", "0.1.0", _,
              [
                formatters: ["html", "epub"],
                deps: _,
                apps: _,
                proglang: :elixir
              ]}
           ] = run(context, ["-f", "html", "-f", "epub"], app: :ex_doc, version: "0.1.0")
  end

  test "accepts multiple formatters from config", context do
    assert [
             {"ex_doc", "0.1.0", _,
              [
                formatters: ["html", "epub"],
                deps: _,
                apps: _,
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
             {"ExDoc", "0.1.0", _,
              [
                formatters: _,
                deps: _,
                apps: _,
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, version: "0.1.0", name: "ExDoc")
  end

  test "accepts modules in :main", context do
    assert [
             {"ex_doc", "dev", _,
              [
                formatters: _,
                deps: _,
                main: "Sample",
                apps: _,
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, docs: [main: Sample])
  end

  test "accepts files in :main", context do
    assert [
             {"ex_doc", "dev", _,
              [
                formatters: _,
                deps: _,
                apps: _,
                main: "another",
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, docs: [main: "another"])
  end

  test "accepts output in :output", %{tmp_dir: tmp_dir} = context do
    assert [
             {"ex_doc", "dev", _,
              [
                formatters: _,
                deps: _,
                apps: _,
                output: output,
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, docs: [output: tmp_dir <> "/hello"])

    assert output == "#{tmp_dir}/hello"
  end

  test "parses output with lower preference than options", %{tmp_dir: tmp_dir} = context do
    output = tmp_dir <> "/world"

    assert [
             {"ex_doc", "dev", _,
              [
                formatters: _,
                deps: _,
                apps: _,
                output: result_output,
                proglang: :elixir
              ]}
           ] = run(context, ["-o", "#{output}"], app: :ex_doc, docs: [output: output])

    assert result_output == "#{tmp_dir}/world"
  end

  test "includes dependencies", context do
    assert [
             {"ex_doc", "dev", _,
              [
                formatters: _,
                deps: deps,
                apps: _,
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, docs: [])

    assert List.keyfind(deps, :earmark_parser, 0) ==
             {:earmark_parser,
              "https://hexdocs.pm/earmark_parser/#{Application.spec(:earmark_parser, :vsn)}/"}
  end

  test "allows custom dependency paths", context do
    assert [
             {"ex_doc", "dev", _,
              [
                formatters: _,
                deps: deps,
                apps: _,
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, docs: [deps: [earmark_parser: "foo"]])

    assert List.keyfind(deps, :earmark_parser, 0) ==
             {:earmark_parser, "foo"}
  end

  test "accepts lazy docs", context do
    assert [
             {"ex_doc", "dev", _,
              [
                formatters: _,
                deps: _,
                apps: _,
                main: "another",
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, docs: fn -> [main: "another"] end)
  end

  test "accepts options from root", context do
    # accepted options are: `app`, `name`, `source_url`, `homepage_url`, `version`
    assert [
             {"ExDoc", "1.2.3-dev", _,
              [
                formatters: _,
                deps: _,
                apps: _,
                description: "it does wonderful things",
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
               proglang: :elixir,
               description: "it does wonderful things"
             )

    assert [{"ex_doc", "dev", _, _}] = run(context, [], app: :ex_doc)
  end

  test "supports umbrella project", context do
    Mix.Project.in_project(:umbrella, "test/fixtures/umbrella", fn _mod ->
      assert [
               {"umbrella", "dev", _,
                [
                  formatters: _,
                  deps: _,
                  apps: [:bar, :foo],
                  proglang: :elixir
                ]}
             ] = run(context, [], app: :umbrella, apps_path: "apps/", docs: [])
    end)
  end

  test "supports umbrella project with ignore_apps", context do
    Mix.Project.in_project(:umbrella, "test/fixtures/umbrella", fn _mod ->
      assert [
               {"umbrella", "dev", _,
                [
                  formatters: _,
                  deps: _,
                  apps: [:bar],
                  ignore_apps: [:foo],
                  proglang: :elixir
                ]}
             ] = run(context, [], app: :umbrella, apps_path: "apps/", docs: [ignore_apps: [:foo]])
    end)
  end

  test "accepts warnings_as_errors in :warnings_as_errors", context do
    assert [
             {"ex_doc", "dev", _,
              [
                formatters: ["html", "markdown", "epub"],
                deps: _,
                apps: [:ex_doc],
                warnings_as_errors: false,
                proglang: :elixir
              ]}
           ] = run(context, [], app: :ex_doc, docs: [warnings_as_errors: false])
  end
end
