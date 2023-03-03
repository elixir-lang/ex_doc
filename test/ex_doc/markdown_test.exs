defmodule ExDoc.MarkdownTest do
  use ExUnit.Case, async: true

  test "sectioninize" do
    list = [
      {:h1, [], ["H1"], %{}},
      {:h2, [class: "example"], ["H2-1"], %{}},
      {:p, [], ["p1"], %{}},
      {:h3, [], ["H3-1"], %{}},
      {:p, [], ["p2"], %{}},
      {:h3, [], ["H3-2"], %{}},
      {:p, [], ["p3"], %{}},
      {:h3, [], ["H3-3"], %{}},
      {:p, [], ["p4"], %{}},
      {:h2, [], ["H2-2"], %{}},
      {:p, [], ["p5"], %{}},
      {:h3, [class: "last"], ["H3-1"], %{}},
      {:p, [], ["p6"], %{}}
    ]

    assert ExDoc.Markdown.sectionize(list, &h2_or_h3?/1) ==
             [
               {:h1, [], ["H1"], %{}},
               {:section, [class: "h2 example"],
                [
                  {:h2, [class: "example"], ["H2-1"], %{}},
                  {:p, [], ["p1"], %{}},
                  {:section, [class: "h3"],
                   [
                     {:h3, [], ["H3-1"], %{}},
                     {:p, [], ["p2"], %{}}
                   ], %{}},
                  {:section, [class: "h3"],
                   [
                     {:h3, [], ["H3-2"], %{}},
                     {:p, [], ["p3"], %{}}
                   ], %{}},
                  {:section, [class: "h3"],
                   [
                     {:h3, [], ["H3-3"], %{}},
                     {:p, [], ["p4"], %{}}
                   ], %{}}
                ], %{}},
               {:section, [class: "h2"],
                [
                  {:h2, [], ["H2-2"], %{}},
                  {:p, [], ["p5"], %{}},
                  {:section, [class: "h3 last"],
                   [
                     {:h3, [class: "last"], ["H3-1"], %{}},
                     {:p, [], ["p6"], %{}}
                   ], %{}}
                ], %{}}
             ]
  end

  defp h2_or_h3?({:h2, _, _, _}), do: true
  defp h2_or_h3?({:h3, _, _, _}), do: true
  defp h2_or_h3?(_), do: false
end
