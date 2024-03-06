defmodule ExDoc.UtilsTest do
  use ExUnit.Case, async: true

  doctest ExDoc.Utils
  alias ExDoc.Utils

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

    assert Utils.natural_sort_by(input, & &1) == ~w|
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

    assert map |> Utils.to_json() |> IO.iodata_to_binary() == Jason.encode!(map)

    string = for i <- 0..0x1F, do: <<i>>, into: ""
    assert string |> Utils.to_json() |> IO.iodata_to_binary() == Jason.encode!(string)
  end

  test "source_url_pattern" do
    assert Utils.source_url_pattern("/%{path}-%{line}", "lib/foo", 13) == "/lib/foo-13"
    assert Utils.source_url_pattern(&"/#{&1}:#{&2}", "lib/foo", 13) == "/lib/foo:13"
  end

  test "strip_tags" do
    assert Utils.strip_tags("<em>Hello</em> World!<br/>") == "Hello World!"
    assert Utils.strip_tags("Go <a href=\"#top\" class='small' disabled>back</a>") == "Go back"
    assert Utils.strip_tags("Git opts (<code class=\"inline\">:git</code>)") == "Git opts (:git)"
    assert Utils.strip_tags("<p>P1.</p><p>P2</p>") == "P1.P2"
    assert Utils.strip_tags("<p>P1.</p><p>P2</p>", " ") == " P1.  P2 "
    assert Utils.strip_tags("<%= @inner_content %>", " ") == "<%= @inner_content %>"
  end

  test "text_to_id" do
    assert Utils.text_to_id("“Stale”") == "stale"
    assert Utils.text_to_id("José") == "josé"
    assert Utils.text_to_id(" a - b ") == "a-b"
    assert Utils.text_to_id(" ☃ ") == ""
    assert Utils.text_to_id(" &sup2; ") == ""
    assert Utils.text_to_id(" &#9180; ") == ""
    assert Utils.text_to_id("Git opts (<code class=\"inline\">:git</code>)") == "git-opts-git"
  end
end
