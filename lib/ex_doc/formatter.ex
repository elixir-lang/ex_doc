defmodule ExDoc.Formatter do
  @moduledoc false

  @doc """
  Autolinks and renders all docs.
  """
  def render_all(project_nodes, extras, ext, config, opts \\ []) do
    base = [
      apps: config.apps,
      deps: config.deps,
      ext: ext,
      extras: extra_paths(extras),
      skip_undefined_reference_warnings_on: config.skip_undefined_reference_warnings_on,
      skip_code_autolink_to: config.skip_code_autolink_to,
      filtered_modules: config.filtered_modules
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

        docs_groups =
          for group <- node.docs_groups do
            docs =
              for child_node <- group.docs do
                id = id(node, child_node)

                autolink_opts =
                  autolink_opts ++
                    [
                      id: id,
                      line: child_node.doc_line,
                      file: child_node.doc_file,
                      current_kfa: {child_node.type, child_node.name, child_node.arity}
                    ]

                specs =
                  Enum.map(child_node.source_specs, &language.autolink_spec(&1, autolink_opts))

                child_node = %{child_node | specs: specs}
                render_doc(child_node, language, autolink_opts, opts)
              end

            %{render_doc(group, language, autolink_opts, opts) | docs: docs}
          end

        %{
          render_doc(node, language, [{:id, node.id} | autolink_opts], opts)
          | docs_groups: docs_groups
        }
      end,
      timeout: :infinity
    )
    |> Enum.map(&elem(&1, 1))
  end

  @doc """
  Autolinks extras.
  """
  def autolink_extras(extras, ext, config) do
    language =
      case config.proglang do
        :erlang -> ExDoc.Language.Erlang
        _ -> ExDoc.Language.Elixir
      end

    autolink_opts = [
      apps: config.apps,
      deps: config.deps,
      ext: ext,
      extras: extra_paths(extras),
      language: language,
      skip_undefined_reference_warnings_on: config.skip_undefined_reference_warnings_on,
      skip_code_autolink_to: config.skip_code_autolink_to
    ]

    extras
    |> Task.async_stream(
      &autolink_extra(&1, language, autolink_opts, []),
      timeout: :infinity
    )
    |> Enum.map(&elem(&1, 1))
  end

  # Helper functions

  defp render_doc(%{doc: nil} = node, _language, _autolink_opts, _opts),
    do: node

  defp render_doc(%{doc: doc} = node, language, autolink_opts, opts) do
    doc = autolink_and_highlight(doc, language, autolink_opts, opts)
    %{node | doc: doc}
  end

  defp autolink_extra(%ExDoc.Extras.URL{} = extra, _language, _autolink_opts, _opts),
    do: extra

  defp autolink_extra(%ExDoc.Extras.Page{doc: nil} = extra, _language, _autolink_opts, _opts),
    do: extra

  defp autolink_extra(
         %ExDoc.Extras.Page{doc: doc, source_path: source_path, id: id} = extra,
         language,
         autolink_opts,
         opts
       ) do
    autolink_opts = [file: source_path, id: id] ++ autolink_opts
    doc = autolink_and_highlight(doc, language, autolink_opts, opts)
    %{extra | doc: doc}
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

  defp autolink_and_highlight(doc, language, autolink_opts, opts) do
    doc
    |> language.autolink_doc(autolink_opts)
    |> ExDoc.DocAST.highlight(language, opts)
  end

  defp extra_paths(extras) do
    Enum.reduce(extras, %{}, fn
      %ExDoc.Extras.URL{}, acc ->
        acc

      %ExDoc.Extras.Page{source_path: source_path, id: id}, acc when is_binary(source_path) ->
        base = Path.basename(source_path)
        Map.put(acc, base, id)

      _extra, acc ->
        acc
    end)
  end

  @doc """
  Generate assets from configs with the given default assets.
  """
  def generate_assets(namespace, defaults, %{output: output, assets: assets}) do
    namespaced_assets =
      if is_map(assets) do
        Enum.map(assets, fn {source, target} -> {source, Path.join(namespace, target)} end)
      else
        ExDoc.Utils.warn(
          """
          giving a binary to :assets is deprecated, please give a map from source to target instead:

              #{inspect(assets: %{assets => "assets"})}
          """,
          []
        )

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
          |> Enum.reduce([], fn path, acc ->
            # Omit directories in .build file
            if File.dir?(path) do
              acc
            else
              [Path.relative_to(path, output) | acc]
            end
          end)
          |> Enum.reverse()

        is_binary(dir_or_files) ->
          []

        true ->
          raise ":assets must be a map of source directories to target directories"
      end
    end)
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

  def generate_image(output, dir, image, name) do
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
end
