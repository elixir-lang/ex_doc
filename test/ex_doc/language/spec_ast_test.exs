defmodule ExDoc.Language.SpecASTTest do
  use ExUnit.Case, async: true
  alias ExDoc.Language.SpecAST

  test "it works" do
    ast =
      quote do
        f(x) :: %{a: atom()} when x: number()
      end

    format_fun = fn ast -> ast |> Macro.to_string() |> Code.format_string!() end

    autolink_fun = fn
      {:type, :atom, 0} ->
        "ATOM"

      {:type, :number, 0} ->
        "NUMBER"
    end

    assert SpecAST.to_string(ast, format_fun, autolink_fun) ==
             "f(x) :: %{a: ATOM()} when x: NUMBER()"
  end
end
