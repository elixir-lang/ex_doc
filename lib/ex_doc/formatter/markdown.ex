defmodule ExDoc.Formatter.Markdown do
  @moduledoc false

  alias __MODULE__.Assets
  alias __MODULE__.Templates
  alias ExDoc.GroupMatcher
  alias ExDoc.Markdown
  alias ExDoc.Utils

  # @main "api-reference"
  @assets_dir "assets"

  @doc """
  Generates Markdown documentation for the given modules.
  """
  @spec run([ExDoc.ModuleNode.t()], [ExDoc.ModuleNode.t()], ExDoc.Config.t()) :: String.t()
  def run(project_nodes, filtered_modules, config) when is_map(config) do
    Utils.unset_warned()

    config = %{config | output: Path.expand(config.output)}
    build = Path.join(config.output, ".build")
    output_setup(build, config)

    project_nodes = render_all(project_nodes, filtered_modules, ".md", config, [])
    extras = build_extras(config, ".md")
    # Generate search early on without api reference in extras
    static_files = generate_assets(".", default_assets(config), config)

    # TODO: Move this categorization to the language
    nodes_map = %{
      modules: filter_list(:module, project_nodes),
      tasks: filter_list(:task, project_nodes)
    }

    extras =
      if config.api_reference do
        [build_api_reference(nodes_map, config) | extras]
      else
        extras
      end

    all_files =
      (static_files ++
         generate_extras(extras, config) ++
         generate_logo(@assets_dir, config) ++
         generate_list(nodes_map.modules, config) ++
         generate_list(nodes_map.tasks, config))
      |> Enum.uniq()
      |> Kernel.--([@assets_dir])
      |> Enum.sort()

    generate_build(all_files, build)

    config.output
    |> Path.join("index.md")
    |> Path.relative_to_cwd()
  end

  @doc """
  Autolinks and renders all docs.
  """
  def render_all(project_nodes, filtered_modules, ext, config, opts) do
    base = [
      apps: config.apps,
      deps: config.deps,
      ext: ext,
      extras: extra_paths(config),
      skip_undefined_reference_warnings_on: config.skip_undefined_reference_warnings_on,
      skip_code_autolink_to: config.skip_code_autolink_to,
      filtered_modules: filtered_modules
    ]

    project_nodes
    |> Task.async_stream(
      fn node ->
        language = node.language

        autolink_opts =
          [
            current_module: node.module,
            file: node.moduledoc_file,
            line: node.moduledoc_line,
            module_id: node.id,
            language: language
          ] ++ base

        docs =
          for child_node <- node.docs do
            id = id(node, child_node)

            autolink_opts =
              autolink_opts ++
                [
                  id: id,
                  line: child_node.doc_line,
                  file: child_node.doc_file,
                  current_kfa: {child_node.type, child_node.name, child_node.arity}
                ]

            specs = Enum.map(child_node.specs, &language.format_spec(&1))
            child_node = %{child_node | specs: specs}
            render_doc(child_node, language, autolink_opts, opts, 4)
          end

        %{
          render_doc(node, language, [{:id, node.id} | autolink_opts], opts, 2)
          | docs: docs
        }
      end,
      timeout: :infinity
    )
    |> Enum.map(&elem(&1, 1))
  end

  defp render_doc(%{doc: nil} = node, _language, _autolink_opts, _opts, _base_heading),
    do: node

  defp render_doc(
         %{doc: _doc, source_doc: source_doc} = node,
         _language,
         _autolink_opts,
         _opts,
         base_heading
       ) do
    # rendered = autolink_and_render(doc, language, autolink_opts, opts)
    rendered = rewrite_headings(source_doc["en"], base_heading)
    %{node | rendered_doc: rendered}
  end

  defp rewrite_headings(markdown, base_heading)
       when is_binary(markdown) and is_integer(base_heading) and base_heading >= 1 do
    {:ok, document} = MDEx.parse_document(markdown)

    document =
      case find_lowest_heading(document) do
        lowest_heading when lowest_heading >= base_heading ->
          document

        lowest_heading ->
          levels_to_bump = base_heading - lowest_heading

          bump_levels(document, levels_to_bump)
      end

    MDEx.to_commonmark!(document)
  end

  defp find_lowest_heading(document) when is_struct(document, MDEx.Document) do
    Enum.reduce_while(document, 6, fn
      %MDEx.Heading{level: 1}, _lowest_level ->
        {:halt, 1}

      %MDEx.Heading{level: level}, lowest_level when level < lowest_level ->
        {:cont, level}

      _, lowest_level ->
        {:cont, lowest_level}
    end)
  end

  defp bump_levels(%MDEx.Document{nodes: nodes} = document, levels_to_bump) do
    nodes_updated =
      Enum.reduce(nodes, [], fn
        %MDEx.Heading{level: level} = heading, acc ->
          updated_element = %{heading | level: increase_level(level, levels_to_bump)}

          [updated_element | acc]

        elem, acc ->
          [elem | acc]
      end)
      |> Enum.reverse()

    Map.put(document, :nodes, nodes_updated)
  end

  defp increase_level(level, levels_to_bump) do
    min(level + levels_to_bump, 6)
  end

  defp id(%{id: mod_id}, %{id: "c:" <> id}) do
    "c:" <> mod_id <> "." <> id
  end

  defp id(%{id: mod_id}, %{id: "t:" <> id}) do
    "t:" <> mod_id <> "." <> id
  end

  defp id(%{id: mod_id}, %{id: id}) do
    mod_id <> "." <> id
  end

  defp output_setup(build, config) do
    if File.exists?(build) do
      build
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.map(&Path.join(config.output, &1))
      |> Enum.each(&File.rm/1)

      File.rm(build)
    else
      File.rm_rf!(config.output)
      File.mkdir_p!(config.output)
    end
  end

  defp generate_build(files, build) do
    entries = Enum.map(files, &[&1, "\n"])
    File.write!(build, entries)
  end

  defp generate_extras(extras, config) do
    generated_extras =
      extras
      |> with_prev_next()
      |> Enum.map(fn {node, prev, next} ->
        filename = "#{node.id}.md"
        output = "#{config.output}/#{filename}"
        config = set_canonical_url(config, filename)

        refs = %{
          prev: prev && %{path: "#{prev.id}.md", title: prev.title},
          next: next && %{path: "#{next.id}.md", title: next.title}
        }

        extension = node.source_path && Path.extname(node.source_path)
        markdown = Templates.extra_template(config, node, extra_type(extension), refs)

        if File.regular?(output) do
          Utils.warn("file #{Path.relative_to_cwd(output)} already exists", [])
        end

        File.write!(output, markdown)
        filename
      end)

    generated_extras ++ copy_extras(config, extras)
  end

  defp extra_type(".cheatmd"), do: :cheatmd
  defp extra_type(".livemd"), do: :livemd
  defp extra_type(_), do: :extra

  defp copy_extras(config, extras) do
    for %{source_path: source_path, id: id} when source_path != nil <- extras,
        ext = extension_name(source_path),
        ext == ".livemd" do
      output = "#{config.output}/#{id}#{ext}"

      File.copy!(source_path, output)

      output
    end
  end

  defp with_prev_next([]), do: []

  defp with_prev_next([head | tail]) do
    Enum.zip([[head | tail], [nil, head | tail], tail ++ [nil]])
  end

  @doc """
  Generate assets from configs with the given default assets.
  """
  def generate_assets(namespace, defaults, %{output: output, assets: assets}) do
    namespaced_assets =
      if is_map(assets) do
        Enum.map(assets, fn {source, target} -> {source, Path.join(namespace, target)} end)
      else
        IO.warn("""
        giving a binary to :assets is deprecated, please give a map from source to target instead:

            #{inspect(assets: %{assets => "assets"})}
        """)

        [{assets, Path.join(namespace, "assets")}]
      end

    Enum.flat_map(defaults ++ namespaced_assets, fn {dir_or_files, relative_target_dir} ->
      target_dir = Path.join(output, relative_target_dir)
      File.mkdir_p!(target_dir)

      cond do
        is_list(dir_or_files) ->
          Enum.map(dir_or_files, fn {name, content} ->
            target = Path.join(target_dir, name)
            File.write(target, content)
            Path.relative_to(target, output)
          end)

        is_binary(dir_or_files) and File.dir?(dir_or_files) ->
          dir_or_files
          |> File.cp_r!(target_dir, dereference_symlinks: true)
          |> Enum.map(&Path.relative_to(&1, output))

        is_binary(dir_or_files) ->
          []

        true ->
          raise ":assets must be a map of source directories to target directories"
      end
    end)
  end

  defp default_assets(config) do
    [
      {Assets.dist(config.proglang), "dist"}
    ]
  end

  defp build_api_reference(nodes_map, config) do
    title = "API Reference"
    api_reference = Templates.api_reference_template(nodes_map, title)

    %{
      content: api_reference,
      group: nil,
      id: "api-reference",
      source_path: nil,
      source_url: config.source_url,
      title: title
    }
  end

  @doc """
  Builds extra nodes by normalizing the config entries.
  """
  def build_extras(config, ext) do
    groups = config.groups_for_extras

    language =
      case config.proglang do
        :erlang -> ExDoc.Language.Erlang
        _ -> ExDoc.Language.Elixir
      end

    source_url_pattern = config.source_url_pattern

    autolink_opts = [
      apps: config.apps,
      deps: config.deps,
      ext: ext,
      extras: extra_paths(config),
      language: language,
      skip_undefined_reference_warnings_on: config.skip_undefined_reference_warnings_on,
      skip_code_autolink_to: config.skip_code_autolink_to
    ]

    extras =
      config.extras
      |> Task.async_stream(
        &build_extra(&1, groups, language, autolink_opts, source_url_pattern),
        timeout: :infinity
      )
      |> Enum.map(&elem(&1, 1))

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

  defp build_extra({input, input_options}, groups, _language, _autolink_opts, source_url_pattern) do
    input = to_string(input)
    id = input_options[:filename] || input |> filename_to_title() |> Utils.text_to_id()
    source_file = input_options[:source] || input
    opts = [file: source_file, line: 1]

    {source, ast} =
      case extension_name(input) do
        extension when extension in ["", ".txt"] ->
          source = File.read!(input)
          ast = [{:pre, [], "\n" <> source, %{}}]
          {source, ast}

        extension when extension in [".md", ".livemd", ".cheatmd"] ->
          source = File.read!(input)

          ast =
            source
            |> Markdown.to_ast(opts)

          # |> sectionize(extension)

          {source, ast}

        _ ->
          raise ArgumentError,
                "file extension not recognized, allowed extension is either .cheatmd, .livemd, .md, .txt or no extension"
      end

    {title_ast, _ast} =
      case ExDoc.DocAST.extract_title(ast) do
        {:ok, title_ast, ast} -> {title_ast, ast}
        :error -> {nil, ast}
      end

    title_text = title_ast && ExDoc.DocAST.text_from_ast(title_ast)
    title_markdown = title_ast && ExDoc.DocAST.to_string(title_ast)
    # content_markdown = autolink_and_render(ast, language, [file: input] ++ autolink_opts, opts)
    content_markdown = source

    group = GroupMatcher.match_extra(groups, input)
    title = input_options[:title] || title_text || filename_to_title(input)

    source_path = source_file |> Path.relative_to(File.cwd!()) |> String.replace_leading("./", "")
    source_url = Utils.source_url_pattern(source_url_pattern, source_path, 1)

    %{
      source: source,
      content: content_markdown,
      group: group,
      id: id,
      source_path: source_path,
      source_url: source_url,
      title: title,
      title_content: title_markdown || title
    }
  end

  defp build_extra(input, groups, language, autolink_opts, source_url_pattern) do
    build_extra({input, []}, groups, language, autolink_opts, source_url_pattern)
  end

  defp extension_name(input) do
    input
    |> Path.extname()
    |> String.downcase()
  end

  # defp sectionize(ast, ".cheatmd") do
  #   ExDoc.DocAST.sectionize(ast, fn
  #     {:h2, _, _, _} -> true
  #     {:h3, _, _, _} -> true
  #     _ -> false
  #   end)
  # end

  # defp sectionize(ast, _), do: ast

  defp filename_to_title(input) do
    input |> Path.basename() |> Path.rootname()
  end

  @doc """
  Generates the logo from config into the given directory.
  """
  def generate_logo(_dir, %{logo: nil}) do
    []
  end

  def generate_logo(dir, %{output: output, logo: logo}) do
    generate_image(output, dir, logo, "logo")
  end

  @doc """
  Generates the cover from config into the given directory.
  """
  def generate_cover(_dir, %{cover: nil}) do
    []
  end

  def generate_cover(dir, %{output: output, cover: cover}) do
    generate_image(output, dir, cover, "cover")
  end

  defp generate_image(output, dir, image, name) do
    extname =
      image
      |> Path.extname()
      |> String.downcase()

    if extname in ~w(.png .jpg .jpeg .svg) do
      filename = Path.join(dir, "#{name}#{extname}")
      target = Path.join(output, filename)
      File.mkdir_p!(Path.dirname(target))
      File.copy!(image, target)
      [filename]
    else
      raise ArgumentError, "image format not recognized, allowed formats are: .png, .jpg, .svg"
    end
  end

  def filter_list(:module, nodes) do
    Enum.filter(nodes, &(&1.type != :task))
  end

  def filter_list(type, nodes) do
    Enum.filter(nodes, &(&1.type == type))
  end

  defp generate_list(nodes, config) do
    nodes
    |> Task.async_stream(&generate_module_page(&1, config), timeout: :infinity)
    |> Enum.map(&elem(&1, 1))
  end

  defp generate_module_page(module_node, config) do
    filename = "#{module_node.id}.md"
    config = set_canonical_url(config, filename)
    content = Templates.module_page(module_node, config)
    File.write!("#{config.output}/#{filename}", content)
    filename
  end

  defp set_canonical_url(config, filename) do
    if config.canonical do
      canonical_url =
        config.canonical
        |> String.trim_trailing("/")
        |> Kernel.<>("/" <> filename)

      Map.put(config, :canonical, canonical_url)
    else
      config
    end
  end

  defp extra_paths(config) do
    Map.new(config.extras, fn
      path when is_binary(path) ->
        base = Path.basename(path)
        {base, Utils.text_to_id(Path.rootname(base))}

      {path, opts} ->
        base = path |> to_string() |> Path.basename()
        {base, opts[:filename] || Utils.text_to_id(Path.rootname(base))}
    end)
  end
end
