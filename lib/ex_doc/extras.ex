defmodule ExDoc.Extras do
  @moduledoc false

  alias ExDoc.{GroupMatcher, Markdown, Utils}

  defmodule Page do
    @moduledoc false

    defstruct id: nil,
              title: nil,
              title_doc: nil,
              group: nil,
              type: nil,
              doc: nil,
              source_doc: nil,
              source_path: nil,
              source_url: nil,
              search_data: nil

    @type t :: %__MODULE__{
            id: String.t(),
            title: String.t(),
            title_doc: ExDoc.DocAST.t() | String.t(),
            group: atom() | nil,
            type: atom(),
            doc: ExDoc.DocAST.t() | nil,
            source_doc: String.t(),
            source_path: String.t(),
            source_url: String.t(),
            search_data: [map()] | nil
          }
  end

  defmodule URL do
    @moduledoc false

    defstruct id: nil,
              title: nil,
              group: nil,
              url: nil

    @type t :: %__MODULE__{
            id: String.t(),
            title: String.t(),
            group: atom() | nil,
            url: String.t()
          }
  end

  @doc """
  Builds extras from config.

  Does not perform autolinking.
  """
  def build(config) do
    groups = config.groups_for_extras
    source_url_pattern = config.source_url_pattern

    extras =
      config.extras
      |> Enum.map(&build_extra(&1, groups, source_url_pattern))

    ids_count = Enum.reduce(extras, %{}, &Map.update(&2, &1.id, 1, fn c -> c + 1 end))

    extras
    |> Enum.map_reduce(1, fn extra, idx ->
      if ids_count[extra.id] > 1, do: {disambiguate_id(extra, idx), idx + 1}, else: {extra, idx}
    end)
    |> elem(0)
    |> Enum.sort_by(fn extra -> GroupMatcher.index(groups, extra.group) end)
  end

  defp disambiguate_id(extra, discriminator) do
    Map.put(extra, :id, "#{extra.id}-#{discriminator}")
  end

  defp build_extra(input, groups, source_url_pattern) when is_binary(input) do
    build_extra({input, %{}}, groups, source_url_pattern)
  end

  defp build_extra({input, opts}, groups, source_url_pattern) when is_list(opts) do
    build_extra({input, Map.new(opts)}, groups, source_url_pattern)
  end

  defp build_extra({input, %{url: _} = input_options}, groups, _source_url_pattern) do
    input = to_string(input)
    title = validate_extra_string!(input_options, :title) || input
    url = validate_extra_string!(input_options, :url)
    group = GroupMatcher.match_extra(groups, url)

    %ExDoc.Extras.URL{
      group: group,
      id: Utils.text_to_id(title),
      title: title,
      url: url
    }
  end

  defp build_extra({input, input_options}, groups, source_url_pattern) do
    input = to_string(input)

    id =
      validate_extra_string!(input_options, :filename) ||
        input |> filename_to_title() |> Utils.text_to_id()

    source_file = validate_extra_string!(input_options, :source) || input
    opts = [file: source_file, line: 1]

    {extension, source, ast} =
      case extension_name(input) do
        extension when extension in ["", ".txt"] ->
          source = File.read!(input)
          ast = [{:pre, [], ["\n" <> source], %{}}]
          {extension, source, ast}

        extension when extension in [".md", ".livemd", ".cheatmd"] ->
          source = File.read!(input)

          ast =
            source
            |> Markdown.to_ast(opts)
            |> ExDoc.DocAST.add_ids_to_headers([:h2, :h3])

          {extension, source, ast}

        _ ->
          raise ArgumentError,
                "file extension not recognized, allowed extension is either .cheatmd, .livemd, .md, .txt or no extension"
      end

    {title_doc, title_text, ast} =
      case ExDoc.DocAST.extract_title(ast) do
        {:ok, title_doc, ast} -> {title_doc, ExDoc.DocAST.text(title_doc), ast}
        :error -> {nil, nil, ast}
      end

    title =
      validate_extra_string!(input_options, :title) || title_text || filename_to_title(input)

    group = GroupMatcher.match_extra(groups, input)
    source_path = source_file |> Path.relative_to(File.cwd!()) |> String.replace_leading("./", "")
    source_url = source_url_pattern.(source_path, 1)
    search_data = validate_search_data!(input_options[:search_data])

    %ExDoc.Extras.Page{
      type: extra_type(extension),
      source_doc: source,
      group: group,
      id: id,
      doc: ast,
      source_path: source_path,
      source_url: source_url,
      search_data: search_data,
      title: title,
      title_doc: title_doc || title
    }
  end

  defp validate_extra_string!(input_options, key) do
    case input_options[key] do
      nil ->
        nil

      binary when is_binary(binary) ->
        binary

      other ->
        raise ArgumentError,
              "extra field #{inspect(key)} must be a string, got: #{inspect(other)}"
    end
  end

  @search_data_keys [:anchor, :body, :title, :type]

  defp validate_search_data!(nil), do: nil

  defp validate_search_data!(search_data) when is_list(search_data) do
    Enum.each(search_data, fn search_data ->
      has_keys = Map.keys(search_data)

      if Enum.sort(has_keys) != @search_data_keys do
        raise ArgumentError,
              "expected search data to be a list of maps with the keys: #{inspect(@search_data_keys)}, " <>
                "found keys: #{inspect(has_keys)}"
      end
    end)

    search_data
  end

  defp validate_search_data!(search_data) do
    raise ArgumentError,
          "expected search data to be a list of maps with the keys: #{inspect(@search_data_keys)}, " <>
            "found: #{inspect(search_data)}"
  end

  defp extension_name(input) do
    input
    |> Path.extname()
    |> String.downcase()
  end

  defp filename_to_title(input) do
    input |> Path.basename() |> Path.rootname()
  end

  defp extra_type(".cheatmd"), do: :cheatmd
  defp extra_type(".livemd"), do: :livemd
  defp extra_type(_), do: :extra
end
