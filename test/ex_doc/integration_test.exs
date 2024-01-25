defmodule ExDoc.IntegrationTest do
  use ExUnit.Case, async: true

  @moduletag :tmp_dir

  test "warnings", %{tmp_dir: dir} do
    File.write!("#{dir}/mix.exs", """
    defmodule MyApp.MixProject do
      use Mix.Project

      def project do
        [
          app: :myapp,
          version: "1.0.0",
          deps: [
            {:ex_doc, path: "#{__DIR__}/../.."}
          ]
        ]
      end
    end
    """)

    File.mkdir!("#{dir}/_build")
    File.cp_r!("#{__DIR__}/../../_build/dev", "#{dir}/_build/dev")

    File.mkdir!("#{dir}/lib")

    File.write!("#{dir}/lib/myapp.ex", ~S'''
    defmodule MyApp do
      @spec foo() :: String.bad()
      def foo(), do: :ok
    end
    ''')

    Mix.Project.in_project(:myapp, dir, fn _mod ->
      File.cp!("#{__DIR__}/../../mix.lock", "#{dir}/mix.lock")

      path =
        Path.wildcard("#{__DIR__}/../../_build/dev/lib/**/ebin")
        |> Enum.join(":")

      options =
        [
          env: %{
            "MIX_DEPS_PATH" => "#{__DIR__}/../../deps",
            "MIX_PATH" => path
          },
          stderr_to_stdout: true
        ]

      {_, 0} = System.cmd("mix", ["compile"], options)
      {_, 0} = System.cmd("mix", ["docs", "-f", "html"], [into: IO.stream()] ++ options)
    end)
  end
end
