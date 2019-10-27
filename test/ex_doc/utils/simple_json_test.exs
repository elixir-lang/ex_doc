defmodule ExDoc.Utils.SimpleJSONTest do
  use ExUnit.Case, async: true

  test "encode" do
    assert %{
             nil: nil,
             true: true,
             false: false,
             atom: :hello,
             string: "world",
             string_with_quotes: "hello \" world",
             list: [
               %{foo: "bar"},
               %{baz: "bat"}
             ],
             integer: 1
           }
           |> ExDoc.Utils.SimpleJSON.encode()
           |> IO.iodata_to_binary() == """
           {\
           "atom":"hello",\
           "false":false,\
           "integer":1,\
           "list":[{"foo":"bar"},{"baz":"bat"}],\
           "nil":null,\
           "string":"world",\
           "string_with_quotes":"hello \\" world",\
           "true":true\
           }\
           """
  end
end
