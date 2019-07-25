defmodule ExDoc.TranslationTest do
  use ExUnit.Case, async: true

  import ExDoc.Translation

  doctest ExDoc.Translation

  defp config(opts \\ []) when is_list(opts) do
    struct(ExDoc.Config, opts)
  end

  test "translate/2" do
    assert translate(config(), "API Reference") == "API Reference"
    assert translate(config(language: "es"), "API Reference") == "Referencia de la API"
    assert translate(config(language: "xyz"), "API Reference") == "API Reference"
  end

  test "translate/3" do
    warning_string = "warning: \#{filename} redirects to \#{redirect_to}, which does not exist"

    assert translate(config(), warning_string,
             filename: "foo.html",
             redirect_to: "bar.html"
           ) == "warning: foo.html redirects to bar.html, which does not exist"

    assert translate(config(language: "es"), warning_string,
             filename: "foo.html",
             redirect_to: "bar.html"
           ) == "advertencia foo.html redirige a bar.html el cual no existe"

    assert translate(config(language: "xyz"), warning_string,
             filename: "foo.html",
             redirect_to: "bar.html"
           ) == "warning: foo.html redirects to bar.html, which does not exist"
  end

  test "replace/2" do
    string = "\#{foo} is foo, \#{bar} is bar, \#{foo} again is foo"
    args = [foo: "foo", bar: "bar"]
    assert replace(string, args) == "foo is foo, bar is bar, foo again is foo"
    assert replace(string, []) == string
    assert replace("hello", args) == "hello"
    assert replace("hello", []) == "hello"
    assert replace("", args) == ""
    assert replace("", []) == ""
  end
end
