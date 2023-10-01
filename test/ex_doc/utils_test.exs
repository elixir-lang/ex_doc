defmodule ExDoc.UtilsTest do
  use ExUnit.Case, async: true
  doctest ExDoc.Utils

  test "natural_sort_by" do
    input = ~w|
      reraise/2
      reraise/3
      send/2
      sigil_C/2
      sigil_D/2
      sigil_c/2
      λ/1
      spawn/1
      spawn/3
      Λ/1
      abc/1
      a2/1
      a1/1
      a0/1
      a10/1
      a1_0/1
      a_0/1
      äpfeln/1
      Äpfeln/1
      умножить/1
      Вычесть/1
      вычесть/1
      rm?/1
      rm!/1
      rm/1
    |

    assert ExDoc.Utils.natural_sort_by(input, & &1) == ~w|
      Äpfeln/1
      a0/1
      a1/1
      a1_0/1
      a2/1
      a10/1
      a_0/1
      abc/1
      äpfeln/1
      reraise/2
      reraise/3
      rm/1
      rm!/1
      rm?/1
      send/2
      sigil_C/2
      sigil_c/2
      sigil_D/2
      spawn/1
      spawn/3
      Λ/1
      λ/1
      Вычесть/1
      вычесть/1
      умножить/1
    |
  end

  test "to_json" do
    map = %{
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

    assert map |> ExDoc.Utils.to_json() |> IO.iodata_to_binary() == Jason.encode!(map)

    string = for i <- 0..0x1F, do: <<i>>, into: ""
    assert string |> ExDoc.Utils.to_json() |> IO.iodata_to_binary() == Jason.encode!(string)
  end

  test "source_url_pattern" do
    assert ExDoc.Utils.source_url_pattern("/%{path}-%{line}", "lib/foo", 13) == "/lib/foo-13"
    assert ExDoc.Utils.source_url_pattern(&"/#{&1}:#{&2}", "lib/foo", 13) == "/lib/foo:13"
  end
end
