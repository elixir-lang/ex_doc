defmodule ExDoc.EPUB.Entities do
  @moduledoc false

  path = Path.join(__DIR__, "htmlmathml-f.ent")
  @external_resource path

  @entities path
            |> File.read!()
            |> then(&Regex.scan(~r/^<!ENTITY\s+(\S+)\s+"([^"]*)"/m, &1))
            |> Map.new(fn [_, name, replacement] ->
              {name, String.replace(replacement, "&#38;#", "&#")}
            end)

  @max_name_byte_size @entities |> Map.keys() |> Enum.map(&byte_size/1) |> Enum.max()
  @named_entity Regex.compile!("&([A-Za-z][A-Za-z0-9]{0,#{@max_name_byte_size - 1}});")

  def to_numeric(html) do
    Regex.replace(@named_entity, html, fn entity, name ->
      if name in ~w(amp apos gt lt quot) do
        entity
      else
        Map.get(@entities, name, "&amp;#{name};")
      end
    end)
  end
end
